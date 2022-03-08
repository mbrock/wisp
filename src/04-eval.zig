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

const Status = enum { val, exp };

pub const Bot = packed struct {
    err: u32 = nil,
    env: u32,
    way: u32,
    exp: u32,
    val: u32,

    pub fn init(exp: u32) Bot {
        return .{
            .way = wisp.top,
            .env = nil,
            .val = nah,
            .exp = exp,
        };
    }
};

ctx: *Ctx,
bot: *Bot,

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
const nah = wisp.nah;

const tidy = @import("./03-tidy.zig");
const read = @import("./05-read.zig").read;
const dump = @import("./06-dump.zig");
const xops = @import("./07-xops.zig");

const Eval = @This();

pub fn step(this: *Eval) !void {
    if (wtf) {
        std.log.warn("\n", .{});
        // try dump.warn("way", this.ctx, this.bot.way);
        try dump.warn("env", this.ctx, this.bot.env);
    }

    const exp = this.bot.exp;
    const val = this.bot.val;

    if (val == nah) {
        if (wtf) try dump.warn("exp", this.ctx, exp);
        switch (wisp.tagOf(exp)) {
            .int, .v08, .sys => this.give(.val, exp),
            .sym => return this.findVariable(exp),
            .duo => return this.intoPair(exp),
            else => return Oof.Bug,
        }
    } else {
        if (wtf) try dump.warn("val", this.ctx, val);
        return this.proceed(val);
    }
}

fn findVariable(this: *Eval, sym: u32) !void {
    var cur = this.bot.env;
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
            const err = [2]u32{
                this.ctx.kwd.@"UNBOUND-VARIABLE",
                sym,
            };
            this.bot.err = try this.ctx.newv32(&err);
            return Oof.Err;
        },
        else => |x| {
            this.give(.val, x);
        },
    }
}

pub fn fail(this: *Eval, xs: []const u32) !void {
    this.bot.err = try this.ctx.newv32(xs);
    return Oof.Err;
}

fn intoPair(this: *Eval, p: u32) !void {
    const duo = try this.ctx.row(.duo, p);
    const car = duo.car;
    const kwd = this.ctx.kwd;

    switch (try this.ctx.get(.sym, .fun, car)) {
        nil => return this.fail(&[_]u32{
            kwd.@"UNDEFINED-FUNCTION",
            car,
        }),
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
    this.give(.exp, duo.car);
    this.bot.way = try this.ctx.new(.ktx, .{
        .hop = this.bot.way,
        .env = this.bot.env,
        .fun = fun,
        .acc = nil,
        .arg = duo.cdr,
    });
}

fn intoFunction(
    this: *Eval,
    fun: u32,
    arg: u32,
) !void {
    if (arg == nil) {
        try this.call(this.bot.way, fun, nil, false);
    } else {
        try this.iter(fun, arg);
    }
}

fn intoMacro(this: *Eval, fun: u32, arg: u32) !void {
    const way = try this.ctx.new(.duo, .{
        .car = this.bot.env,
        .cdr = this.bot.way,
    });
    try this.call(way, fun, arg, false);
}

pub fn proceed(this: *Eval, x: u32) !void {
    if (this.bot.way == wisp.top) {
        this.give(.val, x);
        return;
    }

    switch (wisp.tagOf(this.bot.way)) {
        .ktx => try this.execKtx(try this.ctx.row(.ktx, this.bot.way)),
        .duo => try this.execDuo(try this.ctx.row(.duo, this.bot.way)),

        else => unreachable,
    }
}

fn execDuo(this: *Eval, duo: wisp.Row(.duo)) !void {
    const val = this.bot.val;

    if (wtf) {
        try dump.warn("macroexpansion", this.ctx, val);
        try dump.warn("old env", this.ctx, this.bot.env);
        try dump.warn("new env", this.ctx, duo.car);
    }

    this.bot.* = .{
        .env = duo.car,
        .way = duo.cdr,
        .exp = val,
        .val = nah,
    };
}

fn scan(
    this: *Eval,
    exp: u32,
    par: u32,
    arg: u32,
    way: u32,
    rev: bool,
) !void {
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
            scope[i * 2 + 1] = try wisp.list(
                this.ctx,
                vals.items[i..vals.items.len],
            );
        } else {
            scope[i * 2 + 0] = x;
            scope[i * 2 + 1] = vals.items[i];
        }
    }

    this.bot.env = try this.ctx.new(.duo, .{
        .car = try this.ctx.newv32(scope),
        .cdr = this.bot.env,
    });

    this.give(.exp, exp);
    this.bot.way = way;
}

/// Perform an application, either by directly calling a builtin
/// or by entering a closure.
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
            this.bot.way = way;
        },

        .fun => {
            const fun = try this.ctx.row(.fun, funptr);
            this.bot.env = fun.env;
            try this.scan(fun.exp, fun.par, args, way, rev);
        },

        .mac => {
            const mac = try this.ctx.row(.mac, funptr);
            this.bot.env = mac.env;
            try this.scan(mac.exp, mac.par, args, way, rev);
        },

        .ktx => {
            var vals = try scanListAlloc(this.ctx, args);
            defer vals.deinit();

            if (vals.items.len != 1) {
                try this.fail(&[_]u32{this.ctx.kwd.@"PROGRAM-ERROR"});
            } else {
                this.bot.way = funptr;
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
                    this.bot.way = funptr;
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

pub fn debug(this: *Eval, txt: []const u8, val: u32) !void {
    try dump.warn(txt, this.ctx, val);
}

const Ktx = struct {
    fn call(this: *Eval, ktx: wisp.Row(.ktx)) !void {
        const acc = try this.ctx.new(.duo, .{
            .car = this.bot.val,
            .cdr = ktx.acc,
        });

        if (ktx.arg == nil) {
            try this.call(ktx.hop, ktx.fun, acc, true);
        } else {
            const argduo = try this.ctx.row(.duo, ktx.arg);
            const way = try this.ctx.new(.ktx, .{
                .hop = ktx.hop,
                .env = ktx.env,
                .fun = ktx.fun,
                .acc = acc,
                .arg = argduo.cdr,
            });

            this.bot.way = way;
            this.give(.exp, argduo.car);
        }
    }

    fn PROGN(this: *Eval, ktx: wisp.Row(.ktx)) !void {
        if (ktx.arg == nil) {
            this.bot.way = ktx.hop;
            this.bot.env = ktx.env;
        } else {
            const argduo = try this.ctx.row(.duo, ktx.arg);
            const way = try this.ctx.new(.ktx, .{
                .hop = ktx.hop,
                .env = ktx.env,
                .fun = ktx.fun,
                .acc = nil,
                .arg = argduo.cdr,
            });

            this.bot.way = way;
            this.give(.exp, argduo.car);
        }
    }

    fn LET(this: *Eval, ktx: wisp.Row(.ktx)) !void {
        // LET (k v1 k1 ... x) ((k e) ...)
        const val = this.bot.val;

        if (ktx.arg == nil) {
            var exp: u32 = undefined;

            const env = try this.scanLetAcc(ktx.env, val, ktx.acc, &exp);

            this.bot.way = ktx.hop;
            this.bot.env = env;
            this.give(.exp, exp);
        } else {
            const valacc = try this.ctx.new(.duo, .{
                .car = val,
                .cdr = ktx.acc,
            });

            const argduo = try this.ctx.row(.duo, ktx.arg);
            const letduo = try this.ctx.row(.duo, argduo.car);
            const letsym = letduo.car;
            const letexp = try this.ctx.get(.duo, .car, letduo.cdr);
            const symacc = try this.ctx.new(.duo, .{
                .car = letsym,
                .cdr = valacc,
            });

            const way = try this.ctx.new(.ktx, .{
                .hop = ktx.hop,
                .env = ktx.env,
                .fun = ktx.fun,
                .acc = symacc,
                .arg = argduo.cdr,
            });

            this.bot.way = way;
            this.bot.env = ktx.env;
            this.give(.exp, letexp);
        }
    }

    fn IF(this: *Eval, ktx: wisp.Row(.ktx)) !void {
        const argduo = try this.ctx.row(.duo, ktx.arg);
        const p = this.bot.val != nil;

        this.bot.way = ktx.hop;
        this.bot.env = ktx.env;
        this.give(.exp, if (p) argduo.car else argduo.cdr);
    }
};

fn scanLetAcc(
    this: *Eval,
    env: u32,
    val: u32,
    acc: u32,
    exp: *u32,
) !u32 {
    // We have evaluated the final value of a LET form.  Now we
    // build up the scope from the accumulated bindings.

    var scope = std.ArrayList(u32).init(this.ctx.orb);
    defer scope.deinit();

    const accduo = try this.ctx.row(.duo, acc);
    const letsym = accduo.car;

    try scope.append(letsym);
    try scope.append(val);

    {
        var curduo = try this.ctx.row(.duo, accduo.cdr);
        while (curduo.cdr != nil) {
            const cdrduo = try this.ctx.row(.duo, curduo.cdr);
            const curval = curduo.car;
            const cursym = cdrduo.car;

            try scope.append(cursym);
            try scope.append(curval);

            curduo = try this.ctx.row(.duo, cdrduo.cdr);
        }

        exp.* = curduo.car;
    }

    return this.ctx.new(.duo, .{
        .car = try this.ctx.newv32(scope.items),
        .cdr = env,
    });
}

pub fn execKtx(this: *Eval, ktx: wisp.Row(.ktx)) !void {
    if (ktx.fun == this.ctx.kwd.PROGN)
        try Ktx.PROGN(this, ktx)
    else if (ktx.fun == this.ctx.kwd.IF)
        try Ktx.IF(this, ktx)
    else if (ktx.fun == this.ctx.kwd.LET)
        try Ktx.LET(this, ktx)
    else switch (wisp.tagOf(ktx.fun)) {
        .jet => return Ktx.call(this, ktx),
        .fun => return Ktx.call(this, ktx),

        else => {
            try dump.warn("exec ktx", this.ctx, ktx.fun);
            unreachable;
        },
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

pub fn scanList(
    ctx: *Ctx,
    buffer: []u32,
    reverse: bool,
    list: u32,
) ![]u32 {
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
    job.bot.val = nah;
    job.bot.exp = nah;

    switch (status) {
        .val => job.bot.val = x,
        .exp => job.bot.exp = x,
    }
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
    if (this.bot.err != nil) return wisp.Oof.Bug;

    var i: u32 = 0;
    while (i < limit) : (i += 1) {
        if (this.bot.way == wisp.top and this.bot.val != wisp.nah) {
            return this.bot.val;
        }

        if (this.step()) {} else |err| {
            if (this.bot.err == wisp.nil) {
                this.bot.err = try this.ctx.newv32(
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

pub fn init(ctx: *Ctx, bot: *Bot) Eval {
    return Eval{
        .ctx = ctx,
        .bot = bot,
    };
}

test "step evaluates string" {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    const exp = try ctx.newv08("foo");
    var bot = Bot.init(exp);
    var exe = init(&ctx, &bot);

    try exe.step();
    try expectEqual(exp, exe.bot.val);
    try expectEqual(nah, exe.bot.exp);
}

test "step evaluates variable" {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    const x = try ctx.intern("X", ctx.base);
    const foo = try ctx.newv08("foo");

    var bot = Bot.init(x);
    var exe = init(&ctx, &bot);

    try ctx.set(.sym, .val, x, foo);

    try exe.step();
    try expectEqual(foo, exe.bot.val);
    try expectEqual(nah, exe.bot.exp);
}

pub fn expectEval(want: []const u8, src: []const u8) !void {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    const exp = try read(&ctx, src);
    var bot = Bot.init(exp);
    var exe = init(&ctx, &bot);

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
        try dump.warn("Error", &ctx, exe.bot.err);
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

// test "quasiquote" {
//     try expectEval("(foo 1)",
//         \\ (let ((x 1))
//         \\   `(foo ,x))
//     );
// }

test "prty.lisp" {
    var ctx = try newTestCtx();
    defer ctx.deinit();

    try ctx.load(@embedFile("./06a-prty.lisp"));
}
