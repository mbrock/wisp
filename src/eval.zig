const std = @import("std");

const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const wisp = @import("./wisp.zig");

const Vat = wisp.Vat;
const Ptr = wisp.Ptr;

const read = @import("./read.zig").read;
const Print = @import("./print.zig");
const Primops = @import("./primops.zig");

const Eval = @This();

const Status = enum { done, work };

const Term = union(Status) {
    done: u32,
    work: u32,
};

vat: *Vat,
term: Term,
scopes: u32,
plan: u32,

pub const Error = error{
    Nope,
    EvaluationLimitExceeded,
};

pub fn doneWithTerm(this: *Eval, x: u32) void {
    this.term = .{ .done = x };
}

pub fn step(this: *Eval) !void {
    switch (this.term) {
        .done => |x| {
            return this.proceed(x);
        },

        .work => |t| {
            switch (wisp.tagOf(t)) {
                .int, .str => this.doneWithTerm(t),
                .sym => return this.findVariable(t),
                .duo => return this.stepCons(t),
                else => return Error.Nope,
            }
        },
    }
}

fn findVariable(this: *Eval, word: u32) !void {
    const value = this.vat.tabs.sym.field(.val)[Ptr.from(word).idx];
    if (value == wisp.zap) {
        return Error.Nope;
    } else {
        this.doneWithTerm(value);
    }
}

fn stepCons(this: *Eval, p: u32) !void {
    const cons = try this.vat.get(.duo, p);
    const cdr = try this.vat.get(.duo, cons.cdr);
    const callee = this.vat.tabs.sym.field(.fun)[Ptr.from(cons.car).idx];

    switch (wisp.tagOf(callee)) {
        .fop => {
            this.* = .{
                .vat = this.vat,
                .scopes = this.scopes,
                .term = .{ .work = cdr.car },
                .plan = try this.vat.new(.ct0, .{
                    .hop = this.plan,
                    .env = this.scopes,
                    .fun = callee,
                    .arg = wisp.nil,
                    .exp = cdr.cdr,
                }),
            };
        },

        .mop => {
            const result = try callOp(
                this,
                Primops.mops.values[wisp.Imm.from(callee).idx],
                false,
                cons.cdr,
            );

            this.* = .{
                .vat = this.vat,
                .scopes = this.scopes,
                .term = .{ .work = result },
                .plan = this.plan,
            };
        },

        else => {
            std.log.warn("callee {any}", .{wisp.tagOf(callee)});
            return Error.Nope;
        },
    }
}

pub fn proceed(this: *Eval, x: u32) !void {
    if (this.plan == wisp.nil) {
        this.term = .{ .done = x };
        return;
    }

    switch (wisp.tagOf(this.plan)) {
        .ct0 => try this.doArgsPlan(try this.vat.get(.ct0, this.plan)),
        else => unreachable,
    }
}

fn doArgsPlan(this: *Eval, ct0: wisp.Dat(.ct0)) !void {
    const values = try this.vat.new(.duo, .{
        .car = this.term.done,
        .cdr = ct0.arg,
    });

    if (ct0.exp == wisp.nil) {
        // Done with evaluating subterms.
        switch (wisp.tagOf(ct0.fun)) {
            .fop => {
                const result = try this.callOp(
                    Primops.fops.values[wisp.Imm.from(ct0.fun).idx],
                    true,
                    values,
                );

                this.* = .{
                    .vat = this.vat,
                    .plan = ct0.hop,
                    .scopes = ct0.env,
                    .term = .{ .done = result },
                };
            },
            else => return Error.Nope,
        }
    } else {
        const cons = try this.vat.get(.duo, ct0.exp);
        this.* = .{
            .vat = this.vat,
            .term = .{ .work = cons.car },
            .scopes = this.scopes,
            .plan = try this.vat.new(.ct0, .{
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
        const cons = try vat.get(.duo, cur);
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

fn callOp(this: *Eval, primop: Primops.Op, reverse: bool, values: u32) !u32 {
    switch (primop.tag) {
        .f0x => {
            var xs: [31]u32 = undefined;
            const slice = try scanList(this.vat, &xs, reverse, values);
            const f = Primops.FnTag.f0x.cast(primop.func);
            return try f(this.vat, slice);
        },

        .f2 => {
            var xs: [2]u32 = undefined;
            const slice = try scanList(this.vat, &xs, reverse, values);
            const f = Primops.FnTag.f2.cast(primop.func);
            return try f(this.vat, slice[0], slice[1]);
        },
    }
}

pub fn evaluate(this: *Eval, limit: u32) !u32 {
    var i: u32 = 0;
    while (i < limit) : (i += 1) {
        if (this.plan == wisp.nil) {
            switch (this.term) {
                .done => |x| return x,
                else => {},
            }
        }

        try this.step();
    }

    return Error.EvaluationLimitExceeded;
}

fn newTestVat() !Vat {
    var vat = try Vat.init(std.testing.allocator, .e0);
    try Primops.load(&vat);
    return vat;
}

pub fn init(vat: *Vat, term: u32) Eval {
    return Eval{
        .vat = vat,
        .plan = wisp.nil,
        .scopes = wisp.nil,
        .term = Term{ .work = term },
    };
}

test "step evaluates string" {
    var vat = try newTestVat();
    defer vat.deinit();

    const term = try vat.newstr("foo");
    var ctx = init(&vat, term);

    try ctx.step();
    try expectEqual(Term{ .done = term }, ctx.term);
}

test "step evaluates variable" {
    var vat = try newTestVat();
    defer vat.deinit();

    const x = try vat.intern("X", vat.base());
    const foo = try vat.newstr("foo");

    var ctx = init(&vat, x);

    vat.tabs.sym.field(.val)[Ptr.from(x).idx] = foo;

    try ctx.step();
    try expectEqual(Term{ .done = foo }, ctx.term);
}

fn expectEval(want: []const u8, src: []const u8) !void {
    var vat = try newTestVat();
    defer vat.deinit();

    const term = try read(&vat, src);
    var ctx = init(&vat, term);
    const value = try ctx.evaluate(100);

    const valueString = try Print.printAlloc(
        std.testing.allocator,
        &vat,
        value,
    );

    defer std.testing.allocator.free(valueString);

    const wantValue = try read(&vat, want);
    const wantString = try Print.printAlloc(
        std.testing.allocator,
        &vat,
        wantValue,
    );

    defer std.testing.allocator.free(wantString);

    try expectEqualStrings(wantString, valueString);
}

test "(+ 1 2 3) => 6" {
    try expectEval("6", "(+ 1 2 3)");
}

test "(+ (+ 1 2) (+ 3 4))" {
    try expectEval("10", "(+ (+ 1 2) (+ 3 4))");
}

test "(FOO + 1) => 2" {
    try expectEval("2", "(FOO + 1)");
}
