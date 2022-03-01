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

ctx: *Ctx,
env: u32,
way: u32,
job: Job,

const std = @import("std");

const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const wisp = @import("./ff-wisp.zig");

const Ctx = wisp.Ctx;
const Ptr = wisp.Ptr;
const ref = wisp.ref;
const nil = wisp.nil;

const tidy = @import("./03-tidy.zig");
const read = @import("./05-read.zig").read;
const dump = @import("./06-dump.zig");
const xops = @import("./07-xops.zig");

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
                .int, .v08, .sys => this.doneWithJob(t),
                .sym => return this.findVariable(t),
                .duo => return this.stepDuo(t),
                else => return Error.Nope,
            }
        },
    }
}

fn findVariable(this: *Eval, sym: u32) !void {
    var cur = this.env;
    while (cur != nil) {
        var curduo = try this.ctx.row(.duo, cur);
        var v32 = try this.ctx.v32slice(curduo.car);
        var i: usize = 0;
        while (i < v32.len) : ({
            i += 2;
            cur = curduo.cdr;
        }) {
            if (v32[i] == sym)
                return this.doneWithJob(v32[i + 1]);
        }
    }

    return switch (try this.ctx.get(.sym, .val, sym)) {
        wisp.nah => Error.Nope,
        else => |x| this.doneWithJob(x),
    };
}

const kwds = struct {
    pub fn QUOTE(this: *Eval, cdr: u32) !void {
        this.doneWithJob(try this.ctx.get(.duo, .car, cdr));
    }

    pub fn @"%LET"(this: *Eval, cdr: u32) !void {
        // (%let ((v1 . e1) (v2 . e2)) e)

        var xs: [2]u32 = undefined;
        const args = try scanList(this.ctx, &xs, false, cdr);

        const bs = args[0];
        const e = args[1];

        if (bs == nil) {
            this.job = .{ .exp = e };
        } else {
            // find the first expression
            const b1 = try this.ctx.get(.duo, .car, bs);
            const e1 = try this.ctx.get(.duo, .cdr, b1);
            this.job = .{ .exp = e1 };
            this.way = try this.ctx.new(.ct3, .{
                .hop = this.way,
                .env = this.env,
                .exp = e,
                .arg = bs,
                .dew = nil,
            });
        }
    }

    pub fn @"%LAMBDA"(this: *Eval, cdr: u32) !void {
        var xs: [2]u32 = undefined;
        const args = try scanList(this.ctx, &xs, false, cdr);

        this.job = .{
            .val = try this.ctx.new(.fun, .{
                .env = this.env,
                .par = args[0],
                .exp = args[1],
            }),
        };
    }

    pub fn IF(this: *Eval, cdr: u32) !void {
        var xs: [3]u32 = undefined;
        const args = try scanList(this.ctx, &xs, false, cdr);
        this.* = .{
            .ctx = this.ctx,
            .env = this.env,
            .job = .{ .exp = args[0] },
            .way = try this.ctx.new(.ct1, .{
                .hop = this.way,
                .env = this.env,
                .yay = args[1],
                .nay = args[2],
            }),
        };
    }

    pub fn PROGN(this: *Eval, cdr: u32) !void {
        if (cdr == nil) {
            this.doneWithJob(nil);
        } else {
            const duo = try this.ctx.row(.duo, cdr);
            this.job = .{ .exp = duo.car };
            this.way = try this.ctx.new(.ct2, .{
                .hop = this.way,
                .env = this.env,
                .exp = duo.cdr,
            });
        }
    }
};

fn stepDuo(this: *Eval, p: u32) !void {
    const duo = try this.ctx.row(.duo, p);
    const car = duo.car;
    const cdr = try this.ctx.row(.duo, duo.cdr);
    const kwd = this.ctx.kwd;

    if (car == kwd.IF)
        try kwds.IF(this, duo.cdr)
    else if (car == kwd.QUOTE)
        try kwds.QUOTE(this, duo.cdr)
    else if (car == kwd.PROGN)
        try kwds.PROGN(this, duo.cdr)
    else if (car == kwd.@"%LET")
        try kwds.@"%LET"(this, duo.cdr)
    else if (car == kwd.@"%LAMBDA")
        try kwds.@"%LAMBDA"(this, duo.cdr)
    else switch (try this.ctx.get(.sym, .fun, car)) {
        nil => return Error.Nope,
        else => |fun| try this.stepCall(fun, duo, cdr),
    }
}

fn stepCall(
    this: *Eval,
    fun: u32,
    duo: wisp.Row(.duo),
    cdr: wisp.Row(.duo),
) !void {
    switch (wisp.tagOf(fun)) {
        .fop, .fun => {
            this.* = .{
                .ctx = this.ctx,
                .env = this.env,
                .job = .{ .exp = cdr.car },
                .way = try this.ctx.new(.ct0, .{
                    .hop = this.way,
                    .env = this.env,
                    .fun = fun,
                    .arg = nil,
                    .exp = cdr.cdr,
                }),
            };
        },

        .mop => {
            const result = try callOp(
                this,
                xops.mops.values[wisp.Imm.from(fun).idx],
                false,
                duo.cdr,
            );

            this.* = .{
                .ctx = this.ctx,
                .env = this.env,
                .job = .{ .exp = result },
                .way = this.way,
            };
        },

        else => {
            std.log.warn("callee {any} {any}", .{
                wisp.tagOf(fun),
                wisp.Ptr.from(fun),
            });
            return Error.Nope;
        },
    }
}

pub fn proceed(this: *Eval, x: u32) !void {
    if (this.way == nil) {
        this.job = .{ .val = x };
        return;
    }

    switch (wisp.tagOf(this.way)) {
        .ct0 => try this.execCt0(try this.ctx.row(.ct0, this.way)),
        .ct1 => try this.execCt1(try this.ctx.row(.ct1, this.way)),
        .ct2 => try this.execCt2(try this.ctx.row(.ct2, this.way)),
        .ct3 => try this.execCt3(try this.ctx.row(.ct3, this.way)),

        else => unreachable,
    }
}

fn execCt3(this: *Eval, ct3: wisp.Row(.ct3)) !void {
    const val = this.job.val;
    const argduo = try this.ctx.row(.duo, ct3.arg);
    const sym = try this.ctx.get(.duo, .car, argduo.car);
    const bs = argduo.cdr;

    if (bs == nil) {
        const n = 1 + try wisp.length(this.ctx, ct3.dew);
        var scope = try this.ctx.orb.alloc(u32, 2 * n);
        defer this.ctx.orb.free(scope);

        scope[0] = sym;
        scope[1] = val;

        var cur = ct3.dew;
        var i: u32 = 2;

        while (cur != nil) : (i += 2) {
            const curduo = try this.ctx.row(.duo, cur);
            const b1 = try this.ctx.row(.duo, curduo.car);
            const s1 = b1.car;
            const v1 = b1.cdr;

            scope[i] = s1;
            scope[i + 1] = v1;

            cur = curduo.cdr;
        }

        this.way = ct3.hop;
        this.job = .{ .exp = ct3.exp };
        this.env = try this.ctx.new(.duo, .{
            .car = try this.ctx.newv32(scope),
            .cdr = this.env,
        });
    } else {
        const bsduo = try this.ctx.row(.duo, bs);
        const e1 = try this.ctx.get(.duo, .cdr, bsduo.car);
        this.job = .{ .exp = e1 };
        this.way = try this.ctx.new(.ct3, .{
            .hop = ct3.hop,
            .env = ct3.env,
            .exp = ct3.exp,
            .arg = argduo.cdr,
            .dew = try this.ctx.new(.duo, .{
                .car = try this.ctx.new(.duo, .{ .car = sym, .cdr = val }),
                .cdr = ct3.dew,
            }),
        });
    }
}

fn execCt2(this: *Eval, ct2: wisp.Row(.ct2)) !void {
    const duo = try this.ctx.row(.duo, ct2.exp);

    this.job = .{ .exp = duo.car };

    if (duo.cdr == nil) {
        this.way = ct2.hop;
        this.env = ct2.env;
    } else {
        this.way = try this.ctx.new(.ct2, .{
            .hop = ct2.hop,
            .env = ct2.env,
            .exp = duo.cdr,
        });
    }
}

fn execCt1(this: *Eval, ct1: wisp.Row(.ct1)) !void {
    const exp = if (this.job.val == nil) ct1.nay else ct1.yay;
    this.* = .{
        .ctx = this.ctx,
        .way = ct1.hop,
        .env = ct1.env,
        .job = .{ .exp = exp },
    };
}

fn execCt0(this: *Eval, ct0: wisp.Row(.ct0)) !void {
    const values = try this.ctx.new(.duo, .{
        .car = this.job.val,
        .cdr = ct0.arg,
    });

    if (ct0.exp == nil) {
        // Done with evaluating subterms.
        switch (wisp.tagOf(ct0.fun)) {
            .fop => {
                const result = try this.callOp(
                    xops.fops.values[wisp.Imm.from(ct0.fun).idx],
                    true,
                    values,
                );

                this.* = .{
                    .ctx = this.ctx,
                    .way = ct0.hop,
                    .env = ct0.env,
                    .job = .{ .val = result },
                };
            },

            .fun => {
                const fun = try this.ctx.row(.fun, ct0.fun);

                var xs = std.ArrayList(u32).init(this.ctx.orb);
                defer xs.deinit();

                var curpar = fun.par;
                var curval = values;

                while (curpar != nil) {
                    const parduo = try this.ctx.row(.duo, curpar);
                    const valduo = try this.ctx.row(.duo, curval);

                    try xs.append(parduo.car);
                    try xs.append(valduo.car);

                    curpar = parduo.cdr;
                    curval = valduo.cdr;
                }

                this.* = .{
                    .ctx = this.ctx,
                    .way = ct0.hop,
                    .job = .{ .exp = fun.exp },
                    .env = try this.ctx.new(.duo, .{
                        .car = try this.ctx.newv32(xs.items),
                        .cdr = fun.env,
                    }),
                };
            },

            else => return Error.Nope,
        }
    } else {
        const cons = try this.ctx.row(.duo, ct0.exp);
        this.* = .{
            .ctx = this.ctx,
            .job = .{ .exp = cons.car },
            .env = this.env,
            .way = try this.ctx.new(.ct0, .{
                .hop = ct0.hop,
                .env = ct0.env,
                .fun = ct0.fun,
                .exp = cons.cdr,
                .arg = values,
            }),
        };
    }
}

pub fn scanList(ctx: *Ctx, buffer: []u32, reverse: bool, list: u32) ![]u32 {
    var i: usize = 0;
    var cur = list;
    while (cur != nil) {
        const cons = try ctx.row(.duo, cur);
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

fn callOp(this: *Eval, primop: xops.Op, reverse: bool, values: u32) !u32 {
    switch (primop.tag) {
        .f0x => {
            var xs: [31]u32 = undefined;
            const slice = try scanList(this.ctx, &xs, reverse, values);
            const f = xops.FnTag.f0x.cast(primop.func);
            return try f(this.ctx, slice);
        },

        .f1 => {
            var xs: [1]u32 = undefined;
            const slice = try scanList(this.ctx, &xs, reverse, values);
            const f = xops.FnTag.f1.cast(primop.func);
            return try f(this.ctx, slice[0]);
        },

        .f2 => {
            var xs: [2]u32 = undefined;
            const slice = try scanList(this.ctx, &xs, reverse, values);
            const f = xops.FnTag.f2.cast(primop.func);
            return try f(this.ctx, slice[0], slice[1]);
        },
    }
}

pub fn evaluate(this: *Eval, limit: u32, gc: bool) !u32 {
    var i: u32 = 0;
    while (i < limit) : (i += 1) {
        if (this.way == nil) {
            switch (this.job) {
                .val => |x| return x,
                else => {},
            }
        }

        try this.step();

        if (gc) try tidy.tidyEval(this);
    }

    return Error.EvaluationLimitExceeded;
}

fn newTestCtx() !Ctx {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    try xops.load(&ctx);
    return ctx;
}

pub fn init(ctx: *Ctx, job: u32) Eval {
    return Eval{
        .ctx = ctx,
        .way = nil,
        .env = nil,
        .job = Job{ .exp = job },
    };
}

test "step evaluates string" {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    const exp = try ctx.newv08("foo");
    var exe = init(&ctx, exp);

    try exe.step();
    try expectEqual(Job{ .val = exp }, exe.job);
}

test "step evaluates variable" {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    const x = try ctx.intern("X", ctx.base);
    const foo = try ctx.newv08("foo");

    var exe = init(&ctx, x);

    try ctx.set(.sym, .val, x, foo);

    try exe.step();
    try expectEqual(Job{ .val = foo }, exe.job);
}

fn expectEval(want: []const u8, src: []const u8) !void {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    const exp = try read(&ctx, src);
    var exe = init(&ctx, exp);
    const val = try exe.evaluate(100, true);

    const valueString = try dump.printAlloc(ctx.orb, &ctx, val);

    defer ctx.orb.free(valueString);

    const wantValue = try read(&ctx, want);
    const wantString = try dump.printAlloc(
        ctx.orb,
        &ctx,
        wantValue,
    );

    defer ctx.orb.free(wantString);

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

test "quote" {
    try expectEval("(1 2 3)", "(quote (1 2 3))");
}

test "abbreviated quote" {
    try expectEval("(1 2 3)", "'(1 2 3)");
}

test "let" {
    try expectEval(
        "3",
        "(%let ((a . 1) (b . 2)) (+ a b))",
    );
}

test "calling a closure" {
    try expectEval("13",
        \\ (progn
        \\   (%let ((ten . 10))
        \\     (set-function 'foo (%lambda (x y) (+ ten x y))))
        \\   (foo 1 2))
    );
}
