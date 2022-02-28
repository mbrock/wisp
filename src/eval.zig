//
// This file is part of Wisp.
//
// Wisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// Wisp is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
// or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
// Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with Wisp. If not, see
// <https://www.gnu.org/licenses/>.
//

vat: *Vat,
job: Job,
scopes: u32,
way: u32,

const std = @import("std");

const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const wisp = @import("./wisp.zig");

const Vat = wisp.Vat;
const Ptr = wisp.Ptr;
const ref = wisp.ref;

const read = @import("./read.zig").read;
const Print = @import("./print.zig");
const Ops = @import("./ops.zig");

const Eval = @This();

const Status = enum { val, exp };

const Job = union(Status) {
    val: u32,
    exp: u32,
};

pub const Error = error{
    Nope,
    EvaluationLimitExceeded,
};

pub fn doneWithJob(this: *Eval, x: u32) void {
    this.job = .{ .val = x };
}

pub fn step(this: *Eval) !void {
    switch (this.job) {
        .val => |x| {
            return this.proceed(x);
        },

        .exp => |t| {
            switch (wisp.tagOf(t)) {
                .int, .str => this.doneWithJob(t),
                .sym => return this.findVariable(t),
                .duo => return this.stepDuo(t),
                else => return Error.Nope,
            }
        },
    }
}

fn findVariable(this: *Eval, sym: u32) !void {
    return switch (try this.vat.get(.sym, .val, sym)) {
        wisp.nah => Error.Nope,
        else => |x| this.doneWithJob(x),
    };
}

fn step_IF(this: *Eval, cdr: u32) !void {
    var xs: [3]u32 = undefined;
    const args = try scanList(this.vat, &xs, false, cdr);
    this.* = .{
        .vat = this.vat,
        .scopes = this.scopes,
        .job = .{ .exp = args[0] },
        .way = try this.vat.new(.ct1, .{
            .hop = this.way,
            .env = this.scopes,
            .yay = args[1],
            .nay = args[2],
        }),
    };
}

fn stepDuo(this: *Eval, p: u32) !void {
    const duo = try this.vat.row(.duo, p);
    const car = duo.car;

    if (car == this.vat.specials.IF) {
        return this.step_IF(duo.cdr);
    }

    const fun = try this.vat.get(.sym, .fun, duo.car);
    if (fun == wisp.nil) {
        return Error.Nope;
    }

    const cdr = try this.vat.row(.duo, duo.cdr);

    switch (wisp.tagOf(fun)) {
        .fop => {
            this.* = .{
                .vat = this.vat,
                .scopes = this.scopes,
                .job = .{ .exp = cdr.car },
                .way = try this.vat.new(.ct0, .{
                    .hop = this.way,
                    .env = this.scopes,
                    .fun = fun,
                    .arg = wisp.nil,
                    .exp = cdr.cdr,
                }),
            };
        },

        .mop => {
            const result = try callOp(
                this,
                Ops.mops.values[wisp.Imm.from(fun).idx],
                false,
                duo.cdr,
            );

            this.* = .{
                .vat = this.vat,
                .scopes = this.scopes,
                .job = .{ .exp = result },
                .way = this.way,
            };
        },

        else => {
            std.log.warn("callee {any} {any}", .{ wisp.tagOf(fun), wisp.Ptr.from(fun) });
            return Error.Nope;
        },
    }
}

pub fn proceed(this: *Eval, x: u32) !void {
    if (this.way == wisp.nil) {
        this.job = .{ .val = x };
        return;
    }

    switch (wisp.tagOf(this.way)) {
        .ct0 => try this.execCt0(try this.vat.row(.ct0, this.way)),
        .ct1 => try this.execCt1(try this.vat.row(.ct1, this.way)),
        else => unreachable,
    }
}

fn execCt1(this: *Eval, ct1: wisp.Row(.ct1)) !void {
    const exp = if (this.job.val == wisp.nil) ct1.nay else ct1.yay;
    this.* = .{
        .vat = this.vat,
        .way = ct1.hop,
        .scopes = ct1.env,
        .job = .{ .exp = exp },
    };
}

fn execCt0(this: *Eval, ct0: wisp.Row(.ct0)) !void {
    const values = try this.vat.new(.duo, .{
        .car = this.job.val,
        .cdr = ct0.arg,
    });

    if (ct0.exp == wisp.nil) {
        // Done with evaluating subterms.
        switch (wisp.tagOf(ct0.fun)) {
            .fop => {
                const result = try this.callOp(
                    Ops.fops.values[wisp.Imm.from(ct0.fun).idx],
                    true,
                    values,
                );

                this.* = .{
                    .vat = this.vat,
                    .way = ct0.hop,
                    .scopes = ct0.env,
                    .job = .{ .val = result },
                };
            },
            else => return Error.Nope,
        }
    } else {
        const cons = try this.vat.row(.duo, ct0.exp);
        this.* = .{
            .vat = this.vat,
            .job = .{ .exp = cons.car },
            .scopes = this.scopes,
            .way = try this.vat.new(.ct0, .{
                .hop = ct0.hop,
                .env = ct0.env,
                .fun = ct0.fun,
                .exp = cons.cdr,
                .arg = values,
            }),
        };
    }
}

pub fn scanList(vat: *Vat, buffer: []u32, reverse: bool, list: u32) ![]u32 {
    var i: usize = 0;
    var cur = list;
    while (cur != wisp.nil) {
        const cons = try vat.row(.duo, cur);
        buffer[i] = cons.car;
        cur = cons.cdr;
        i += 1;
    }

    var slice = buffer[0..i];
    if (reverse) {
        std.mem.reverse(u32, slice);
    }
    return slice;
}

fn callOp(this: *Eval, primop: Ops.Op, reverse: bool, values: u32) !u32 {
    switch (primop.tag) {
        .f0x => {
            var xs: [31]u32 = undefined;
            const slice = try scanList(this.vat, &xs, reverse, values);
            const f = Ops.FnTag.f0x.cast(primop.func);
            return try f(this.vat, slice);
        },

        .f1 => {
            var xs: [1]u32 = undefined;
            const slice = try scanList(this.vat, &xs, reverse, values);
            const f = Ops.FnTag.f1.cast(primop.func);
            return try f(this.vat, slice[0]);
        },

        .f2 => {
            var xs: [2]u32 = undefined;
            const slice = try scanList(this.vat, &xs, reverse, values);
            const f = Ops.FnTag.f2.cast(primop.func);
            return try f(this.vat, slice[0], slice[1]);
        },
    }
}

pub fn evaluate(this: *Eval, limit: u32) !u32 {
    var i: u32 = 0;
    while (i < limit) : (i += 1) {
        if (this.way == wisp.nil) {
            switch (this.job) {
                .val => |x| return x,
                else => {},
            }
        }

        try this.step();
    }

    return Error.EvaluationLimitExceeded;
}

fn newTestVat() !Vat {
    var vat = try Vat.init(std.testing.allocator, .e0);
    try Ops.load(&vat);
    return vat;
}

pub fn init(vat: *Vat, job: u32) Eval {
    return Eval{
        .vat = vat,
        .way = wisp.nil,
        .scopes = wisp.nil,
        .job = Job{ .exp = job },
    };
}

test "step evaluates string" {
    var vat = try newTestVat();
    defer vat.deinit();

    const exp = try vat.newstr("foo");
    var ctx = init(&vat, exp);

    try ctx.step();
    try expectEqual(Job{ .val = exp }, ctx.job);
}

test "step evaluates variable" {
    var vat = try newTestVat();
    defer vat.deinit();

    const x = try vat.intern("X", vat.base);
    const foo = try vat.newstr("foo");

    var ctx = init(&vat, x);

    try vat.set(.sym, .val, x, foo);

    try ctx.step();
    try expectEqual(Job{ .val = foo }, ctx.job);
}

fn expectEval(want: []const u8, src: []const u8) !void {
    var vat = try newTestVat();
    defer vat.deinit();

    const exp = try read(&vat, src);
    var ctx = init(&vat, exp);
    const val = try ctx.evaluate(100);

    const valueString = try Print.printAlloc(vat.orb, &vat, val);

    defer vat.orb.free(valueString);

    const wantValue = try read(&vat, want);
    const wantString = try Print.printAlloc(
        vat.orb,
        &vat,
        wantValue,
    );

    defer vat.orb.free(wantString);

    try expectEqualStrings(wantString, valueString);
}

test "(+ 1 2 3) => 6" {
    try expectEval("6", "(+ 1 2 3)");
}

test "(+ (+ 1 2) (+ 3 4))" {
    try expectEval("10", "(+ (+ 1 2) (+ 3 4))");
}

test "(foo + 1) => 2" {
    try expectEval("2", "(foo + 1)");
}

test "(car (cons 1 2)) => 1" {
    try expectEval("1", "(car (cons 1 2))");
}

test "(cdr (cons 1 2)) => 2" {
    try expectEval("2", "(cdr (cons 1 2))");
}

test "nil => nil" {
    try expectEval("nil", "nil");
}

test "if" {
    try expectEval("0", "(if nil 1 0)");
    try expectEval("1", "(if t 1 0)");
}

test "progn" {
    try expectEval("3", "(progn 1 2 3)");
}

test "prog1" {
    try expectEval("1", "(prog1 1 2 3)");
}
