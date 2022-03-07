// -*- fill-column: 64; -*-
//
// This file is part of Wisp.
//
// Wisp is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version
// 3 of the License, or (at your option) any later version.
//
// Wisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General
// Public License along with Wisp. If not, see
// <https://www.gnu.org/licenses/>.
//

pub var wtf = false;

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
    if (wtf) {
        std.log.warn("\n", .{});
        // try dump.warn("way", this.ctx, this.way);
        try dump.warn("env", this.ctx, this.env);
    }
    switch (this.job) {
        .val => |x| {
            if (wtf) try dump.warn("val", this.ctx, x);
            return this.proceed(x);
        },

        .exp => |t| {
            if (wtf) try dump.warn("exp", this.ctx, t);
            switch (wisp.tagOf(t)) {
                .int, .v08, .sys => this.give(.val, t),
                .sym => return this.findVariable(t),
                .duo => return this.intoPair(t),
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
        while (i < v32.len) : (i += 2) {
            if (v32[i] == sym)
                return this.give(.val, v32[i + 1]);
        }
        cur = curduo.cdr;
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

pub fn fail(this: *Eval, xs: []const u32) !void {
    this.err = try this.ctx.newv32(xs);
    return Oof.Err;
}

fn intoPair(this: *Eval, p: u32) !void {
    const duo = try this.ctx.row(.duo, p);
    const car = duo.car;
    const kwd = this.ctx.kwd;

    switch (try this.ctx.get(.sym, .fun, car)) {
        nil => return this.fail(&[_]u32{ kwd.@"UNDEFINED-FUNCTION", car }),
        else => |fun| try this.intoCall(fun, duo.cdr),
    }
}

fn intoCall(
    this: *Eval,
    fun: u32,
    arg: u32,
) !void {
    return switch (wisp.tagOf(fun)) {
        .jet => this.intoJet(fun, arg),
        .fun => this.intoFunction(fun, arg),
        .mac => this.intoMacro(fun, arg),
        else => Oof.Bug,
    };
}

fn intoJet(this: *Eval, fun: u32, arg: u32) !void {
    const idx = wisp.Imm.from(fun).idx;
    const jet = xops.jets[idx];

    switch (jet.ilk) {
        .fun => {
            if (arg == nil) {
                try this.oper(fun, nil, false);
            } else {
                try this.iter(fun, arg);
            }
        },

        .ctl => {
            try this.oper(fun, arg, false);
        },
    }
}

fn iter(this: *Eval, fun: u32, arg: u32) !void {
    // Start evaluating the first argument with a
    // continuation of the remaining arguments.
    const duo = try this.ctx.row(.duo, arg);
    this.job = .{ .exp = duo.car };
    this.way = try this.ctx.new(.ct0, .{
        .hop = this.way,
        .env = this.env,
        .fun = fun,
        .arg = nil,
        .exp = duo.cdr,
    });
}

fn intoFunction(
    this: *Eval,
    fun: u32,
    arg: u32,
) !void {
    if (arg == nil) {
        try this.call(this.way, fun, nil, false);
    } else {
        try this.iter(fun, arg);
    }
}

fn intoMacro(this: *Eval, fun: u32, arg: u32) !void {
    const way = try this.ctx.new(.duo, .{
        .car = this.env,
        .cdr = this.way,
    });
    try this.call(way, fun, arg, false);
}

pub fn proceed(this: *Eval, x: u32) !void {
    if (this.way == wisp.top) {
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

    try dump.warn("macroexpansion", this.ctx, val);
    try dump.warn("old env", this.ctx, this.env);
    try dump.warn("new env", this.ctx, duo.car);

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

fn scan(this: *Eval, exp: u32, par: u32, arg: u32, way: u32, rev: bool) !void {
    var pars = try scanListAlloc(this.ctx, par);
    defer pars.deinit();
    var vals = try scanListAlloc(this.ctx, arg);
    defer vals.deinit();

    if (rev)
        std.mem.reverse(u32, vals.items);

    var scope = try this.ctx.orb.alloc(u32, 2 * pars.items.len);
    defer this.ctx.orb.free(scope);

    var i: usize = 0;
    while (i < pars.items.len) : (i += 1) {
        const x = pars.items[i];
        if (x == this.ctx.kwd.@"&REST") {
            scope[i * 2 + 0] = pars.items[i + 1];
            scope[i * 2 + 1] = try wisp.list(this.ctx, vals.items[i..vals.items.len]);
        } else {
            scope[i * 2 + 0] = x;
            scope[i * 2 + 1] = vals.items[i];
        }
    }

    this.env = try this.ctx.new(.duo, .{
        .car = try this.ctx.newv32(scope),
        .cdr = this.env,
    });

    this.job = .{ .exp = exp };
    this.way = way;
}

/// Perform an application, either by directly calling a builtin or by
/// entering a closure.
pub fn call(
    this: *Eval,
    way: u32,
    funptr: u32,
    args: u32,
    rev: bool,
) anyerror!void {
    switch (wisp.tagOf(funptr)) {
        .jet => {
            try this.oper(funptr, args, rev);
            this.way = way;
        },

        .fun => {
            const fun = try this.ctx.row(.fun, funptr);
            this.env = fun.env;
            try this.scan(fun.exp, fun.par, args, way, rev);
        },

        .mac => {
            const mac = try this.ctx.row(.mac, funptr);
            this.env = mac.env;
            try this.scan(mac.exp, mac.par, args, way, rev);
        },

        .ct0, .ct1, .ct2, .ct3 => {
            var vals = try scanListAlloc(this.ctx, args);
            defer vals.deinit();

            if (vals.items.len != 1) {
                try this.fail(&[_]u32{this.ctx.kwd.@"PROGRAM-ERROR"});
            } else {
                this.way = funptr;
                try this.proceed(vals.items[0]);
            }
        },

        .sys => {
            if (funptr == wisp.top) {
                var vals = try scanListAlloc(this.ctx, args);
                defer vals.deinit();

                if (vals.items.len != 1) {
                    try this.fail(
                        &[_]u32{this.ctx.kwd.@"PROGRAM-ERROR"},
                    );
                } else {
                    this.way = funptr;
                    try this.proceed(vals.items[0]);
                }
            } else {
                return Oof.Bug;
            }
        },

        else => {
            try dump.warn("oof", this.ctx, funptr);
            return Oof.Bug;
        },
    }
}

pub fn execCt0(this: *Eval, ct0: wisp.Row(.ct0)) !void {
    const values = try this.ctx.new(.duo, .{
        .car = this.job.val,
        .cdr = ct0.arg,
    });

    if (ct0.exp == nil) {
        try this.call(ct0.hop, ct0.fun, values, true);
    } else {
        const duo = try this.ctx.row(.duo, ct0.exp);
        this.* = .{
            .ctx = this.ctx,
            .job = .{ .exp = duo.car },
            .env = this.env,
            .way = try this.ctx.new(.ct0, .{
                .hop = ct0.hop,
                .env = ct0.env,
                .fun = ct0.fun,
                .exp = duo.cdr,
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

fn oper(job: *Eval, jet: u32, arg: u32, rev: bool) !void {
    const xop = xops.jets[wisp.Imm.from(jet).idx];
    switch (xop.tag) {
        .f0x => {
            const args = try scanListAlloc(job.ctx, arg);
            defer args.deinit();
            if (rev) std.mem.reverse(u32, args.items);
            const fun = cast(.f0x, xop);
            try fun(job, args.items);
        },

        .f0r => {
            const rest = if (rev) try reverseList(job.ctx, arg) else arg;
            const fun = cast(.f0r, xop);
            try fun(job, .{ .arg = rest });
        },

        .f1r => {
            const list = if (rev) try reverseList(job.ctx, arg) else arg;
            const duo = try job.ctx.row(.duo, list);
            const rest = duo.cdr;
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
                if (rev) std.mem.reverse(u32, args);
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
                if (rev) std.mem.reverse(u32, args);
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
        if (this.way == wisp.top) {
            switch (this.job) {
                .val => |x| return x,
                else => {},
            }
        }

        if (this.step()) {} else |err| {
            if (this.err == wisp.nil) {
                this.err = try this.ctx.newv32(
                    &[_]u32{this.ctx.kwd.@"PROGRAM-ERROR"},
                );
            }
            return err;
        }

        if (gc) try tidy.tidyEval(this);
    }

    try this.fail(&[_]u32{this.ctx.kwd.EXHAUSTED});

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
        .way = wisp.top,
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

    if (exe.evaluate(1_000, true)) |val| {
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
    } else |e| {
        try dump.warn("Error", &ctx, exe.err);
        return e;
    }
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
        \\     (set-symbol-function 'foo (lambda (x y) (+ ten x y))))
        \\   (foo 1 2))
    );
}

test "calling a macro closure" {
    try expectEval("3",
        \\ (progn
        \\   (set-symbol-function 'frob
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
        \\ (funcall (lambda (x y) (cons y x)) 'a 'b)
    );
}

test "APPLY" {
    try expectEval("(a b c)",
        \\ (apply (lambda (x y z) (list x y z)) '(a b c))
    );
}

test "defun with &rest" {
    try expectEval("(x . (1 2 3))",
        \\ (progn
        \\   (defun foo (x &rest xs) (cons x xs))
        \\   (foo 'x 1 2 3))
    );
}

test "quasiquote" {
    try expectEval("(foo 1)",
        \\ (let ((x 1))
        \\   `(foo ,x))
    );
}

test "prty.lisp" {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    try ctx.load(@embedFile("./06a-prty.lisp"));
}
