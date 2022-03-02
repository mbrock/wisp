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

const wtf = false;

ctx: *Ctx,
env: u32,
way: u32,
job: Job,
err: u32 = nil,

const std = @import("std");

const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const wisp = @import("./ff-wisp.zig");

const Oof = wisp.Oof;
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

pub fn step(this: *Eval) !void {
    switch (this.job) {
        .val => |x| {
            if (wtf) {
                try dump.warn("val", this.ctx, x);
                try dump.warn("way", this.ctx, this.way);
            }
            return this.proceed(x);
        },

        .exp => |t| {
            if (wtf) {
                try dump.warn("exp", this.ctx, t);
            }
            switch (wisp.tagOf(t)) {
                .int, .v08, .sys => this.give(.val, t),
                .sym => return this.findVariable(t),
                .duo => return this.stepDuo(t),
                else => return Oof.Bug,
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
                return this.give(.val, v32[i + 1]);
        }
    }

    switch (try this.ctx.get(.sym, .val, sym)) {
        wisp.nah => {
            const err = [2]u32{ this.ctx.kwd.@"UNBOUND-VARIABLE", sym };
            this.err = try this.ctx.newv32(&err);
            return Oof.Err;
        },
        else => |x| {
            this.give(.val, x);
        },
    }
}

const kwds = struct {};

fn fail(this: *Eval, xs: []const u32) !void {
    this.err = try this.ctx.newv32(xs);
    return Oof.Err;
}

fn stepDuo(this: *Eval, p: u32) !void {
    const duo = try this.ctx.row(.duo, p);
    const car = duo.car;
    const kwd = this.ctx.kwd;

    switch (try this.ctx.get(.sym, .fun, car)) {
        nil => return this.fail(&[_]u32{ kwd.@"UNDEFINED-FUNCTION", car }),
        else => |fun| try this.stepCall(fun, duo),
    }
}

fn stepCall(
    this: *Eval,
    fun: u32,
    duo: wisp.Row(.duo),
) !void {
    switch (wisp.tagOf(fun)) {
        .fop, .fun => {
            if (duo.cdr == nil) {
                // No need for argument evaluation.
                try this.apply(this.way, fun, nil, false);
            } else {
                // Start evaluating the first argument with a
                // continuation of the remaining arguments.
                const cdr = try this.ctx.row(.duo, duo.cdr);
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
            }
        },

        .mac => {
            // With macros we don't evaluate arguments.
            const cdr = try this.ctx.row(.duo, duo.cdr);
            const mac = try this.ctx.row(.mac, fun);
            var xs = std.ArrayList(u32).init(this.ctx.orb);
            defer xs.deinit();

            var curpar = mac.par;
            var curval = try this.ctx.new(.duo, cdr);

            while (curpar != nil) {
                const parduo = try this.ctx.row(.duo, curpar);
                const valduo = try this.ctx.row(.duo, curval);

                try xs.append(parduo.car);
                try xs.append(valduo.car);

                curpar = parduo.cdr;
                curval = valduo.cdr;
            }

            const env = try this.ctx.new(.duo, .{
                .car = try this.ctx.newv32(xs.items),
                .cdr = mac.env,
            });

            this.* = .{
                .ctx = this.ctx,
                .job = .{ .exp = mac.exp },
                .env = env,
                .way = try this.ctx.new(.duo, .{
                    .car = this.env,
                    .cdr = this.way,
                }),
            };
        },

        .mop => {
            const mop = xops.mops.values[wisp.Imm.from(fun).idx];
            try this.oper(mop, duo.cdr);
        },

        else => {
            std.log.warn("callee {any} {any}", .{
                wisp.tagOf(fun),
                wisp.Ptr.from(fun),
            });
            return Oof.Bug;
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
        .duo => try this.execDuo(try this.ctx.row(.duo, this.way)),

        else => unreachable,
    }
}

fn execDuo(this: *Eval, duo: wisp.Row(.duo)) !void {
    const val = this.job.val;
    this.* = .{
        .ctx = this.ctx,
        .job = .{ .exp = val },
        .env = duo.car,
        .way = duo.cdr,
    };
}

fn execCt3(this: *Eval, ct3: wisp.Row(.ct3)) !void {
    const val = this.job.val;
    const argduo = try this.ctx.row(.duo, ct3.arg);
    const sym = try this.ctx.get(.duo, .car, argduo.car);
    const bindings = argduo.cdr;

    if (bindings == nil) {
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
        const binding = try this.ctx.row(.duo, bindings);
        const e1 = try this.ctx.get(.duo, .car, try this.ctx.get(.duo, .cdr, binding.car));
        this.job = .{ .exp = e1 };
        this.way = try this.ctx.new(.ct3, .{
            .hop = ct3.hop,
            .env = ct3.env,
            .exp = ct3.exp,
            .arg = bindings,
            .dew = try this.ctx.new(.duo, .{
                .car = try this.ctx.new(.duo, .{ .car = sym, .cdr = val }),
                .cdr = ct3.dew,
            }),
        });
    }
}

fn execCt2(this: *Eval, ct2: wisp.Row(.ct2)) !void {
    if (ct2.exp == nil) {
        this.way = ct2.hop;
        this.env = ct2.env;
        return;
    }

    const duo = try this.ctx.row(.duo, ct2.exp);

    this.job = .{ .exp = duo.car };
    this.way = try this.ctx.new(.ct2, .{
        .hop = ct2.hop,
        .env = ct2.env,
        .exp = duo.cdr,
    });
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

/// Perform an application, either by directly calling a builtin or by
/// entering a closure.
pub fn apply(
    this: *Eval,
    way: u32,
    funptr: u32,
    args: u32,
    /// Ugly hack to cope with FUNCALL.
    noreverse: bool,
) !void {
    switch (wisp.tagOf(funptr)) {
        .fop => {
            const fop = xops.fops.values[wisp.Imm.from(funptr).idx];
            try this.oper(fop, args);
            this.way = way;
        },

        .fun => {
            const fun = try this.ctx.row(.fun, funptr);

            var pars = try scanListAlloc(this.ctx, fun.par);
            defer pars.deinit();
            var vals = try scanListAlloc(this.ctx, args);
            defer vals.deinit();

            if (pars.items.len != vals.items.len) {
                return Oof.Err;
            }

            if (!noreverse)
                std.mem.reverse(u32, vals.items);

            var scope = try this.ctx.orb.alloc(u32, 2 * pars.items.len);
            defer this.ctx.orb.free(scope);

            for (pars.items) |par, i| {
                scope[i * 2 + 0] = par;
                scope[i * 2 + 1] = vals.items[i];
            }

            this.* = .{
                .ctx = this.ctx,
                .way = way,
                .job = .{ .exp = fun.exp },
                .env = try this.ctx.new(.duo, .{
                    .car = try this.ctx.newv32(scope),
                    .cdr = fun.env,
                }),
            };
        },

        else => return Oof.Bug,
    }
}

fn execCt0(this: *Eval, ct0: wisp.Row(.ct0)) !void {
    const values = try this.ctx.new(.duo, .{
        .car = this.job.val,
        .cdr = ct0.arg,
    });

    if (ct0.exp == nil) {
        try this.apply(ct0.hop, ct0.fun, values, false);
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

fn scanListAlloc(ctx: *Ctx, list: u32) !std.ArrayList(u32) {
    var xs = std.ArrayList(u32).init(ctx.orb);
    errdefer xs.deinit();

    var cur = list;
    while (cur != nil) {
        const duo = try ctx.row(.duo, cur);
        try xs.append(duo.car);
        cur = duo.cdr;
    }

    return xs;
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

pub fn give(job: *Eval, status: Status, x: u32) void {
    job.job = switch (status) {
        .val => .{ .val = x },
        .exp => .{ .exp = x },
    };
}

fn cast(
    comptime tag: xops.FnTag,
    xop: xops.Op,
) tag.functionType() {
    return tag.cast(xop.fun);
}

fn reverseList(ctx: *Ctx, list: u32) !u32 {
    var cur = list;
    var rev = nil;
    while (cur != nil) {
        const duo = try ctx.row(.duo, cur);
        rev = try ctx.new(.duo, .{ .car = duo.car, .cdr = rev });
        cur = duo.cdr;
    }
    return rev;
}

fn oper(job: *Eval, xop: xops.Op, arg: u32) !void {
    switch (xop.tag) {
        .f0x => {
            const args = try scanListAlloc(job.ctx, arg);
            defer args.deinit();
            if (xop.ilk == .fun) std.mem.reverse(u32, args.items);
            const fun = cast(.f0x, xop);
            try fun(job, args.items);
        },

        .f0r => {
            var rest = arg;
            if (xop.ilk == .fun) {
                rest = try reverseList(job.ctx, rest);
            }
            const fun = cast(.f0r, xop);
            try fun(job, .{ .arg = rest });
        },

        .f1r => {
            var list = arg;
            if (xop.ilk == .fun) {
                list = try reverseList(job.ctx, list);
            }

            var duo = try job.ctx.row(.duo, list);
            var rest = duo.cdr;
            const fun = cast(.f1r, xop);
            try fun(job, duo.car, .{ .arg = rest });
        },

        .f0 => {
            if (arg == nil) {
                const fun = cast(.f0, xop);
                try fun(job);
            } else {
                try job.fail(&[_]u32{job.ctx.kwd.@"PROGRAM-ERROR"});
            }
        },

        .f1 => {
            const list = try scanListAlloc(job.ctx, arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 1) {
                const fun = cast(.f1, xop);
                try fun(job, args[0]);
            } else {
                try job.fail(&[_]u32{job.ctx.kwd.@"PROGRAM-ERROR"});
            }
        },

        .f2 => {
            const list = try scanListAlloc(job.ctx, arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 2) {
                if (xop.ilk == .fun) std.mem.reverse(u32, args);
                const fun = cast(.f2, xop);
                try fun(job, args[0], args[1]);
            } else {
                try job.fail(&[_]u32{job.ctx.kwd.@"PROGRAM-ERROR"});
            }
        },

        .f3 => {
            const list = try scanListAlloc(job.ctx, arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 3) {
                if (xop.ilk == .fun) std.mem.reverse(u32, args);
                const fun = cast(.f3, xop);
                try fun(job, args[0], args[1], args[2]);
            } else {
                try job.fail(&[_]u32{job.ctx.kwd.@"PROGRAM-ERROR"});
            }
        },
    }
}

pub fn evaluate(this: *Eval, limit: u32, gc: bool) !u32 {
    if (this.err != nil) return wisp.Oof.Bug;

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

    return Oof.Ugh;
}

pub fn newTestCtx() !Ctx {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    try xops.load(&ctx);
    try ctx.cook();
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

pub fn expectEval(want: []const u8, src: []const u8) !void {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    const exp = try read(&ctx, src);
    var exe = init(&ctx, exp);
    const val = try exe.evaluate(1_000_000, true);

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
        "(let ((a 1) (b 2)) (+ a b))",
    );
}

test "calling a closure" {
    try expectEval("13",
        \\ (progn
        \\   (let ((ten 10))
        \\     (set-function 'foo (%lambda (x y) (+ ten x y))))
        \\   (foo 1 2))
    );
}

test "calling a macro closure" {
    try expectEval("3",
        \\ (progn
        \\   (set-function 'frob
        \\      (%macro-lambda (x y z)
        \\        (list y x z)))
        \\   (frob 1 + 2))
    );
}

test "(list 1 2 3)" {
    try expectEval("(1 2 3)", "(list 1 2 3)");
}

test "EQ" {
    try expectEval("T", "(eq 1 1)");
    try expectEval("NIL", "(eq 1 2)");
    try expectEval("T", "(eq 'foo 'foo)");
    try expectEval("NIL", "(eq 'foo 'bar)");
}

test "DEFUN" {
    try expectEval("(1 . 2)",
        \\ (progn (defun f (x y) (cons x y)) (f 1 2))
    );
}

test "base test suite" {
    try expectEval("nil", "(base-test)");
}

test "FUNCALL" {
    try expectEval("(b . a)",
        \\ (funcall (%lambda (x y) (cons y x)) 'a 'b)
    );
}

test "APPLY" {
    try expectEval("(a b c)",
        \\ (apply (%lambda (x y z) (list x y z)) '(a b c))
    );
}
