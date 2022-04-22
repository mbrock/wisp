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

const std = @import("std");

const Wisp = @import("./wisp.zig");
const Tidy = @import("./tidy.zig");
const Sexp = @import("./sexp.zig");
const Jets = @import("./jets.zig");

const Step = @This();
const Heap = Wisp.Heap;

pub const Run = Wisp.Row(.run);

const Status = enum { val, exp };

heap: *Heap,
run: *Run,

pub var wtf = false;

pub fn initRun(exp: u32) Run {
    return .{
        .way = top,
        .env = nil,
        .err = nil,
        .val = nah,
        .exp = exp,
    };
}

const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const Oof = Wisp.Oof;
const Ptr = Wisp.Ptr;
const Row = Wisp.Row;
const nah = Wisp.nah;
const nil = Wisp.nil;
const ref = Wisp.ref;
const t = Wisp.t;
const tagOf = Wisp.tagOf;
const top = Wisp.top;

pub fn once(heap: *Heap, run: *Run) !void {
    var step = Step{ .heap = heap, .run = run };
    step.attemptOneStep() catch |e| try step.handleError(e);
}

fn makeCondition(step: *Step, err: anyerror) !u32 {
    return if (step.run.err == nil)
        try step.heap.newv32(&.{
            step.heap.kwd.@"LOW-LEVEL-ERROR",
            try step.heap.newv08(@errorName(err)),
        })
    else
        step.run.err;
}

pub fn handleError(step: *Step, err: anyerror) !void {
    const condition = try step.makeCondition(err);
    step.run.err = nil;
    try Jets.Funs.@"SEND-WITH-DEFAULT!"(step, step.heap.kwd.ERROR, condition, Wisp.nah);
}

pub fn attemptOneStep(step: *Step) !void {
    const heap = step.heap;
    const run = step.run;
    const exp = run.exp;
    const val = run.val;

    if (wtf) {
        std.log.warn("\n", .{});
        try Sexp.warn("env", heap, run.env);
        try Sexp.warn("ktx", heap, run.way);
    }

    if (val == nah) {
        if (wtf) try Sexp.warn("exp", heap, exp);
        switch (tagOf(exp)) {
            .int, .v08, .v32, .sys => step.give(.val, exp),
            .sym => return step.findVariable(exp),
            .duo => return step.intoPair(exp),
            else => return error.UnknownExpressionTag,
        }
    } else {
        if (wtf) try Sexp.warn("val", heap, val);
        return step.proceed(val);
    }
}

fn findVariable(step: *Step, sym: u32) !void {
    var pkg = try step.heap.get(.sym, .pkg, sym);
    if (pkg == step.heap.keywordPackage) {
        step.run.exp = nah;
        step.run.val = sym;
        return;
    }

    const dyn = try step.heap.get(.sym, .dyn, sym);
    if (dyn != nil) {
        if (try step.findDynamicBinding(sym)) |ktx| {
            step.give(.val, try step.heap.get(.ktx, .arg, ktx));
            return;
        }

        // If dynamic variable has no dynamic binding, treat it
        // as a lexical variable.
    }

    var cur = step.run.env;
    while (cur != nil) {
        var curduo = try step.heap.row(.duo, cur);
        var v32 = try step.heap.v32slice(curduo.car);
        var i: usize = 0;
        while (i < v32.len) : (i += 2) {
            if (v32[i] == sym)
                return step.give(.val, v32[i + 1]);
        }
        cur = curduo.cdr;
    }

    switch (try step.heap.get(.sym, .val, sym)) {
        nah => {
            const err = [2]u32{
                step.heap.kwd.@"UNBOUND-VARIABLE",
                sym,
            };
            step.run.err = try step.heap.newv32(&err);
            return Oof.Err;
        },
        else => |x| {
            step.give(.val, x);
        },
    }
}

pub fn findDynamicBinding(step: *Step, name: u32) !?u32 {
    var cur = step.run.way;

    while (cur != top) {
        const fun = try step.heap.get(.ktx, .fun, cur);
        if (fun == step.heap.kwd.BINDING) {
            const acc = try step.heap.get(.ktx, .acc, cur);
            if (acc == name) {
                return cur;
            }
        }

        cur = try step.heap.get(.ktx, .hop, cur);
    }

    return null;
}

pub fn fail(step: *Step, xs: []const u32) !void {
    step.run.err = try step.heap.newv32(xs);
    return Oof.Err;
}

fn intoPair(step: *Step, p: u32) !void {
    const duo = try step.heap.row(.duo, p);
    const car = duo.car;
    const kwd = step.heap.kwd;

    if (tagOf(car) != .sym) {
        return step.fail(&.{
            kwd.@"INVALID-CALLEE",
            car,
        });
    }

    switch (try step.heap.get(.sym, .fun, car)) {
        nil => return fail(step, &[_]u32{
            kwd.@"UNDEFINED-FUNCTION",
            car,
        }),
        else => |fun| try intoCall(step, fun, duo.cdr),
    }
}

fn intoCall(step: *Step, fun: u32, arg: u32) !void {
    return switch (tagOf(fun)) {
        .jet => intoJet(step, fun, arg),
        .fun => intoFunction(step, fun, arg),
        .mac => intoMacro(step, fun, arg),
        else => error.BadCallTag,
    };
}

fn intoJet(step: *Step, fun: u32, arg: u32) !void {
    const idx = Wisp.Imm.from(fun).idx;
    const jet = Jets.jets[idx];

    switch (jet.ilk) {
        .fun => {
            if (arg == nil) {
                try step.oper(fun, nil, false);
            } else {
                try step.iter(fun, arg);
            }
        },

        .ctl => {
            try oper(step, fun, arg, false);
        },
    }
}

fn iter(step: *Step, fun: u32, arg: u32) !void {
    // Start evaluating the first argument with a
    // continuation of the remaining arguments.
    const duo = try step.heap.row(.duo, arg);
    step.give(.exp, duo.car);
    step.run.way = try step.heap.new(.ktx, .{
        .hop = step.run.way,
        .env = step.run.env,
        .fun = fun,
        .acc = nil,
        .arg = duo.cdr,
    });
}

fn intoFunction(step: *Step, fun: u32, arg: u32) !void {
    if (arg == nil) {
        try step.call(fun, nil, false);
    } else {
        try step.iter(fun, arg);
    }
}

fn intoMacro(step: *Step, fun: u32, arg: u32) !void {
    const way = try step.heap.new(.ktx, .{
        .hop = step.run.way,
        .env = step.run.env,
        .fun = step.heap.kwd.EVAL,
        .acc = nil,
        .arg = nil,
    });

    try step.call(fun, arg, false);

    step.run.way = way;
}

pub fn proceed(step: *Step, x: u32) !void {
    if (step.run.way == top) {
        step.run.env = nil;
        step.give(.val, x);
        return;
    }

    switch (tagOf(step.run.way)) {
        .ktx => try step.execKtx(try step.heap.row(.ktx, step.run.way)),

        else => |tag| {
            std.log.err(
                "cannot proceed with continuation {any}",
                .{tag},
            );

            return error.BadContinuationTag;
        },
    }
}

fn scan(
    step: *Step,
    fun: u32,
    exp: u32,
    par: u32,
    arg: u32,
    rev: bool,
) !void {
    var _tmp = std.heap.stackFallback(4096, step.heap.orb);
    var tmp = _tmp.get();

    var pars = try scanListAlloc(step.heap, tmp, par);
    defer pars.deinit();
    var vals = try scanListAlloc(step.heap, tmp, arg);
    defer vals.deinit();

    if (rev)
        std.mem.reverse(u32, vals.items);

    var scope = try tmp.alloc(u32, 2 * pars.items.len);
    defer tmp.free(scope);

    var i: usize = 0; // how many pars we scanned
    var n: usize = 0; // how many vars we bound
    var m: usize = 0; // how many vals we used

    var optional: bool = false;

    loop: while (i < pars.items.len) : (i += 1) {
        const x = pars.items[i];
        if (x == step.heap.kwd.@"&REST" or x == step.heap.kwd.@"&BODY") {
            scope[n * 2 + 0] = pars.items[i + 1];
            scope[n * 2 + 1] = try Wisp.list(
                step.heap,
                vals.items[m..vals.items.len],
            );

            n += 1;
            m = vals.items.len;

            break :loop;
        } else if (x == step.heap.kwd.@"&OPTIONAL") {
            optional = true;
        } else if (m < vals.items.len) {
            scope[n * 2 + 0] = x;
            scope[n * 2 + 1] = vals.items[m];
            n += 1;
            m += 1;
        } else if (optional) {
            scope[n * 2 + 0] = x;
            scope[n * 2 + 1] = nil;
            n += 1;
        } else {
            try step.fail(&[_]u32{
                step.heap.kwd.@"PROGRAM-ERROR",
                step.heap.kwd.@"INVALID-ARGUMENT-COUNT",
                @intCast(u32, vals.items.len),
                fun,
            });
        }
    }

    if (m < vals.items.len) {
        try step.fail(&[_]u32{
            step.heap.kwd.@"PROGRAM-ERROR",
            step.heap.kwd.@"INVALID-ARGUMENT-COUNT",
            @intCast(u32, vals.items.len),
            fun,
        });
    }

    step.run.env = try step.heap.cons(
        try step.heap.newv32(scope[0 .. n * 2]),
        step.run.env,
    );

    step.give(.exp, exp);
}

/// Perform an application, either by directly calling a builtin
/// or by entering a closure.
pub fn call(
    step: *Step,
    funptr: u32,
    args: u32,
    rev: bool,
) anyerror!void {
    var _tmp = std.heap.stackFallback(4096, step.heap.orb);
    var tmp = _tmp.get();

    if (wtf) {
        try step.warn("call", funptr);
    }

    switch (tagOf(funptr)) {
        .jet => {
            try step.oper(funptr, args, rev);
        },

        .fun => {
            const fun = try step.heap.row(.fun, funptr);
            step.run.env = fun.env;
            try step.scan(funptr, fun.exp, fun.par, args, rev);
            try step.heap.set(.fun, .cnt, funptr, 1 + fun.cnt);
        },

        .mac => {
            const mac = try step.heap.row(.mac, funptr);
            step.run.env = mac.env;
            try step.scan(funptr, mac.exp, mac.par, args, rev);
            try step.heap.set(.mac, .cnt, funptr, 1 + mac.cnt);
        },

        .ktx => {
            var vals = try scanListAlloc(step.heap, tmp, args);
            defer vals.deinit();

            if (vals.items.len != 1) {
                try step.fail(&[_]u32{
                    step.heap.kwd.@"PROGRAM-ERROR",
                    step.heap.kwd.@"CONTINUATION-CALL-ERROR",
                });
            } else {
                step.run.way = try step.composeContinuation(funptr);
                try step.proceed(vals.items[0]);
            }
        },

        .sys => {
            if (funptr == top) {
                var vals = try scanListAlloc(step.heap, tmp, args);
                defer vals.deinit();

                if (vals.items.len != 1) {
                    try step.fail(&.{
                        step.heap.kwd.@"PROGRAM-ERROR",
                        step.heap.kwd.@"CONTINUATION-CALL-ERROR",
                    });
                } else {
                    step.run.way = try step.composeContinuation(funptr);
                    try step.proceed(vals.items[0]);
                }
            } else {
                try step.fail(&.{
                    step.heap.kwd.@"PROGRAM-ERROR",
                    step.heap.kwd.@"CONTINUATION-CALL-ERROR",
                    funptr,
                });
            }
        },

        else => {
            try Sexp.warn("oof", step.heap, funptr);
            return error.BadFunctionTag;
        },
    }
}

pub fn warn(step: *Step, text: []const u8, exp: u32) !void {
    try Sexp.warn(text, step.heap, exp);
}

pub fn composeContinuation(step: *Step, way: u32) !u32 {
    if (way == top) {
        return step.run.way;
    } else {
        var new = try step.heap.copyAny(way);
        var cur = new;

        while (cur != top) {
            cur = switch (tagOf(cur)) {
                .ktx => try lookForTop(step, cur),
                else => return error.BadContinuationTag,
            };
        }

        return new;
    }
}

fn lookForTop(
    step: *Step,
    cur: u32,
) !u32 {
    const hop = try step.heap.get(.ktx, .hop, cur);
    if (hop == top) {
        try step.heap.set(.ktx, .hop, cur, step.run.way);
        return top;
    } else {
        const new = try step.heap.copy(.ktx, hop);
        try step.heap.set(.ktx, .hop, cur, new);
        return new;
    }
}

pub fn debug(heap: *Heap, txt: []const u8, val: u32) !void {
    try Sexp.warn(txt, heap, val);
}

const Ktx = struct {
    fn funargs(step: *Step, ktx: Row(.ktx)) !void {
        const acc = try step.heap.cons(step.run.val, ktx.acc);

        // Come back to the environment of the call form.
        step.run.env = ktx.env;

        if (ktx.arg == nil) {
            step.run.way = ktx.hop;
            try call(step, ktx.fun, acc, true);
        } else {
            const argduo = try step.heap.row(.duo, ktx.arg);
            const way = try step.heap.new(.ktx, .{
                .hop = ktx.hop,
                .env = ktx.env,
                .fun = ktx.fun,
                .acc = acc,
                .arg = argduo.cdr,
            });

            step.run.way = way;
            step.give(.exp, argduo.car);
        }
    }

    fn EVAL(step: *Step, ktx: Row(.ktx)) !void {
        const exp = step.run.val;
        step.run.* = .{
            .err = nil,
            .env = ktx.env,
            .way = ktx.hop,
            .exp = exp,
            .val = nah,
        };
    }

    fn PROMPT(step: *Step, ktx: Row(.ktx)) !void {
        step.run.way = ktx.hop;
        step.run.env = ktx.env;
    }

    fn BINDING(step: *Step, ktx: Row(.ktx)) !void {
        step.run.way = ktx.hop;
        step.run.env = ktx.env;
    }

    fn DO(step: *Step, ktx: Row(.ktx)) !void {
        if (ktx.arg == nil) {
            step.run.way = ktx.hop;
            step.run.env = ktx.env;
        } else {
            const argduo = try step.heap.row(.duo, ktx.arg);

            step.give(.exp, argduo.car);
            step.run.env = ktx.env;
            step.run.way = if (argduo.cdr == nil)
                ktx.hop
            else
                try step.heap.new(.ktx, .{
                    .fun = step.heap.kwd.DO,
                    .env = ktx.env,
                    .acc = nil,
                    .arg = argduo.cdr,
                    .hop = ktx.hop,
                });
        }
    }

    fn LET(step: *Step, ktx: Row(.ktx)) !void {
        // LET (k v1 k1 ... x) ((k e) ...)
        const val = step.run.val;

        if (ktx.arg == nil) {
            var exp: u32 = undefined;

            const env = try scanLetAcc(
                step.heap,
                ktx.env,
                val,
                ktx.acc,
                &exp,
            );

            step.run.way = ktx.hop;
            step.run.env = env;
            step.give(.exp, exp);
        } else {
            const valacc = try step.heap.cons(val, ktx.acc);
            const argduo = try step.heap.row(.duo, ktx.arg);
            const letduo = try step.heap.row(.duo, argduo.car);
            const letsym = letduo.car;
            const letexp = try step.heap.get(.duo, .car, letduo.cdr);
            const symacc = try step.heap.cons(letsym, valacc);

            const way = try step.heap.new(.ktx, .{
                .hop = ktx.hop,
                .env = ktx.env,
                .fun = ktx.fun,
                .acc = symacc,
                .arg = argduo.cdr,
            });

            step.run.way = way;
            step.run.env = ktx.env;
            step.give(.exp, letexp);
        }
    }

    fn IF(step: *Step, ktx: Row(.ktx)) !void {
        const argduo = try step.heap.row(.duo, ktx.arg);
        const p = step.run.val != nil;

        step.run.way = ktx.hop;
        step.run.env = ktx.env;
        step.give(.exp, if (p) argduo.car else argduo.cdr);
    }
};

fn scanLetAcc(
    heap: *Heap,
    env: u32,
    val: u32,
    acc: u32,
    exp: *u32,
) !u32 {
    // We have evaluated the final value of a LET form.  Now we
    // build up the scope from the accumulated bindings.

    var scope = std.ArrayList(u32).init(heap.orb);
    defer scope.deinit();

    const accduo = try heap.row(.duo, acc);
    const letsym = accduo.car;

    try scope.append(letsym);
    try scope.append(val);

    {
        var curduo = try heap.row(.duo, accduo.cdr);
        while (curduo.cdr != nil) {
            const cdrduo = try heap.row(.duo, curduo.cdr);
            const curval = curduo.car;
            const cursym = cdrduo.car;

            try scope.append(cursym);
            try scope.append(curval);

            curduo = try heap.row(.duo, cdrduo.cdr);
        }

        exp.* = curduo.car;
    }

    return heap.cons(try heap.newv32(scope.items), env);
}

pub fn execKtx(step: *Step, ktx: Row(.ktx)) !void {
    if (ktx.fun == step.heap.kwd.DO)
        try Ktx.DO(step, ktx)
    else if (ktx.fun == step.heap.kwd.IF)
        try Ktx.IF(step, ktx)
    else if (ktx.fun == step.heap.kwd.LET)
        try Ktx.LET(step, ktx)
    else if (ktx.fun == step.heap.kwd.PROMPT)
        try Ktx.PROMPT(step, ktx)
    else if (ktx.fun == step.heap.kwd.BINDING)
        try Ktx.BINDING(step, ktx)
    else if (ktx.fun == step.heap.kwd.EVAL)
        try Ktx.EVAL(step, ktx)
    else switch (tagOf(ktx.fun)) {
        .jet => return Ktx.funargs(step, ktx),
        .fun => return Ktx.funargs(step, ktx),

        else => {
            try Sexp.warn("exec ktx", step.heap, ktx.fun);
            unreachable;
        },
    }
}

pub const ListKind = enum { proper, dotted };
pub const List = union(ListKind) {
    proper: std.ArrayList(u32),
    dotted: std.ArrayList(u32),

    pub fn isDotted(this: List) bool {
        return switch (this) {
            .proper => false,
            .dotted => true,
        };
    }

    pub fn arrayList(this: List) std.ArrayList(u32) {
        return switch (this) {
            .proper => |xs| xs,
            .dotted => |xs| xs,
        };
    }

    pub fn deinit(this: List) void {
        this.arrayList().deinit();
    }
};

pub fn scanListAlloc(heap: *Heap, tmp: Wisp.Orb, list: u32) !std.ArrayList(u32) {
    return switch (try scanListAllocAllowDotted(heap, tmp, list)) {
        .proper => |xs| xs,
        .dotted => Oof.Err,
    };
}

pub fn scanListAllocAllowDotted(heap: *Heap, tmp: Wisp.Orb, list: u32) !List {
    var xs = try std.ArrayList(u32).initCapacity(tmp, 16);
    errdefer xs.deinit();

    var cur = list;
    while (tagOf(cur) == .duo) {
        const duo = try heap.row(.duo, cur);
        try xs.append(duo.car);
        cur = duo.cdr;
    }

    if (cur == nil) {
        return List{ .proper = xs };
    } else {
        try xs.append(cur);
        return List{ .dotted = xs };
    }
}

pub fn scanList(
    heap: *Heap,
    buffer: []u32,
    reverse: bool,
    list: u32,
) ![]u32 {
    var i: usize = 0;
    var cur = list;
    while (cur != nil) {
        const cons = try heap.row(.duo, cur);
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

pub fn give(step: *Step, status: Status, x: u32) void {
    step.run.val = nah;
    step.run.exp = nah;

    switch (status) {
        .val => step.run.val = x,
        .exp => step.run.exp = x,
    }
}

fn cast(
    comptime tag: Jets.FnTag,
    jet: Jets.Op,
) tag.functionType() {
    return tag.cast(jet.fun);
}

fn reverseList(heap: *Heap, list: u32) !u32 {
    var cur = list;
    var rev = nil;
    while (cur != nil) {
        const duo = try heap.row(.duo, cur);
        rev = try heap.cons(duo.car, rev);
        cur = duo.cdr;
    }
    return rev;
}

fn invalidArgumentCount(step: *Step, fun: u32) !void {
    try step.fail(&[_]u32{
        step.heap.kwd.@"PROGRAM-ERROR",
        step.heap.kwd.@"INVALID-ARGUMENT-COUNT",
        fun,
    });
}

pub fn failTypeMismatch(step: *Step, arg: u32, expected: u32) !void {
    try step.fail(&.{
        step.heap.kwd.@"PROGRAM-ERROR",
        step.heap.kwd.@"TYPE-MISMATCH",
        expected,
        arg,
    });
}

fn oper(step: *Step, jet: u32, arg: u32, rev: bool) !void {
    if (step.invokeJet(jet, arg, rev)) {
        return;
    } else |err| {
        const condition = if (step.run.err == nil)
            try step.heap.newv32(&.{
                step.heap.kwd.@"LOW-LEVEL-ERROR",
                try step.heap.newv08(@errorName(err)),
            })
        else
            step.run.err;

        step.run.err = try step.heap.newv32(&.{
            step.heap.kwd.@"BUILTIN-FAILURE",
            jet,
            condition,
        });

        return err;
    }
}

fn invokeJet(step: *Step, jet: u32, arg: u32, rev: bool) !void {
    const def = Jets.jets[Wisp.Imm.from(jet).idx];
    var tmp = std.heap.stackFallback(4096, step.heap.orb);

    switch (def.tag) {
        .f0x => {
            const args = try scanListAlloc(step.heap, tmp.get(), arg);
            defer args.deinit();
            if (rev) std.mem.reverse(u32, args.items);
            const fun = cast(.f0x, def);
            try fun(step, args.items);
        },

        .f0r => {
            const rest = if (rev) try reverseList(step.heap, arg) else arg;
            const fun = cast(.f0r, def);
            try fun(step, .{ .arg = rest });
        },

        .f1r => {
            const list = if (rev) try reverseList(step.heap, arg) else arg;
            const duo = try step.heap.row(.duo, list);
            const rest = duo.cdr;
            const fun = cast(.f1r, def);
            try fun(step, duo.car, .{ .arg = rest });
        },

        .f1x => {
            const args = try scanListAlloc(step.heap, tmp.get(), arg);
            defer args.deinit();
            if (rev) std.mem.reverse(u32, args.items);
            const fun = cast(.f1x, def);
            try fun(step, args.items[0], args.items[1..args.items.len]);
        },

        .f2x => {
            const args = try scanListAlloc(step.heap, tmp.get(), arg);
            defer args.deinit();
            if (args.items.len < 2) {
                return step.invalidArgumentCount(jet);
            } else {
                if (rev) std.mem.reverse(u32, args.items);
                const fun = cast(.f2x, def);
                try fun(
                    step,
                    args.items[0],
                    args.items[1],
                    args.items[2..args.items.len],
                );
            }
        },

        .f0 => {
            if (arg == nil) {
                const fun = cast(.f0, def);
                try fun(step);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f1 => {
            const list = try scanListAlloc(step.heap, tmp.get(), arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 1) {
                const fun = cast(.f1, def);
                try fun(step, args[0]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f2 => {
            const list = try scanListAlloc(step.heap, tmp.get(), arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 2) {
                if (rev) std.mem.reverse(u32, args);
                const fun = cast(.f2, def);
                try fun(step, args[0], args[1]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f3 => {
            const list = try scanListAlloc(step.heap, tmp.get(), arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 3) {
                if (rev) std.mem.reverse(u32, args);
                const fun = cast(.f3, def);
                try fun(step, args[0], args[1], args[2]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f4 => {
            const list = try scanListAlloc(step.heap, tmp.get(), arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 4) {
                if (rev) std.mem.reverse(u32, args);
                const fun = cast(.f4, def);
                try fun(step, args[0], args[1], args[2], args[3]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        // XXX: I know this is horrible.  I'm going to refactor it later...
        .f5 => {
            const list = try scanListAlloc(step.heap, tmp.get(), arg);
            const args = list.items;
            defer list.deinit();

            if (args.len == 5) {
                if (rev) std.mem.reverse(u32, args);
                const fun = cast(.f5, def);
                try fun(step, args[0], args[1], args[2], args[3], args[4]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },
    }
}

pub fn stepOver(heap: *Heap, run: *Run, limit: u32) !void {
    const breakpoint = run.way;
    _ = try evaluateUntilSpecificContinuation(
        heap,
        run,
        limit,
        breakpoint,
    );

    try once(heap, run);
}

pub fn getParentContinuation(heap: *Heap, way: u32) !u32 {
    return switch (tagOf(way)) {
        .ktx => heap.get(.ktx, .hop, way),
        .duo => heap.get(.duo, .cdr, way),
        else => error.BadParentContinuation,
    };
}

pub fn stepOut(heap: *Heap, run: *Run, limit: u32) !void {
    const breakpoint = try getParentContinuation(heap, run.way);
    _ = try evaluateUntilSpecificContinuation(
        heap,
        run,
        limit,
        breakpoint,
    );
}

pub fn evaluateUntilSpecificContinuation(
    heap: *Heap,
    run: *Run,
    limit: u32,
    breakpoint: u32,
) !u32 {
    if (run.err != nil) return error.ErrorAlreadyPresent;

    var step = Step{ .heap = heap, .run = run };

    var i: u32 = 0;
    while (true) {
        if (limit > 0 and i >= limit) break;

        if (!heap.inhibit_gc and (heap.please_tidy or (limit == 0 and i > 0 and @mod(i, 100_000) == 0))) {
            // var timer = try std.time.Timer.start();
            // const s0 = heap.bytesize();

            var gc = try prepareToTidy(&step);
            try finishTidying(&step, &gc);

            // const s1 = heap.bytesize();
            // const nanoseconds = timer.read();

            // if (s0 - s1 > 1_000) {
            // try std.io.getStdErr().writer().print(
            //     ";; [gc took {d}ms; {d} KB to {d} KB]\n",
            //     .{
            //         @intToFloat(f64, nanoseconds) / 1_000_000,
            //         s0 / 1024,
            //         s1 / 1024,
            //     },
            // );
            // }

            heap.please_tidy = false;
        }

        if ((run.way == breakpoint or run.way == top) and run.val != nah) {
            return run.val;
        }

        if (once(heap, run)) {
            i += 1;
        } else |err| {
            if (run.err == nil) {
                run.err = try heap.newv32(
                    &[_]u32{
                        heap.kwd.@"LOW-LEVEL-ERROR",
                        try step.heap.newv08(@errorName(err)),
                    },
                );
            }
            return err;
        }
    }

    try step.fail(&[_]u32{step.heap.kwd.EXHAUSTED});

    return Oof.Ugh;
}

pub fn evaluate(heap: *Heap, run: *Run, limit: u32) !u32 {
    return evaluateUntilSpecificContinuation(
        heap,
        run,
        limit,
        top,
    );
}

pub fn prepareToTidy(step: *Step) !Tidy {
    var gc = try Tidy.init(step.heap);
    try gc.root();
    try gc.move(&step.run.err);
    try gc.move(&step.run.env);
    try gc.move(&step.run.way);
    try gc.move(&step.run.val);
    try gc.move(&step.run.exp);

    for (step.heap.roots.items) |x| {
        try gc.move(x);
    }

    return gc;
}

pub fn finishTidying(step: *Step, gc: *Tidy) !void {
    try gc.scan();
    step.heap.* = gc.done();
}

pub fn newTestHeap() !Heap {
    var heap = try Heap.init(std.testing.allocator, .e0);
    try Jets.load(&heap);
    try heap.cookBase();
    return heap;
}

test "step evaluates string" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const exp = try heap.newv08("foo");
    var run = initRun(exp);

    try once(&heap, &run);
    try expectEqual(exp, run.val);
    try expectEqual(nah, run.exp);
}

test "step evaluates variable" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const x = try heap.intern("X", heap.base);
    const foo = try heap.newv08("foo");

    var run = initRun(x);

    try heap.set(.sym, .val, x, foo);

    try once(&heap, &run);

    try expectEqual(foo, run.val);
    try expectEqual(nah, run.exp);
}

pub fn evalString(heap: *Heap, src: []const u8) !u32 {
    const exp = try Sexp.read(heap, src);
    var run = initRun(exp);

    if (evaluate(heap, &run, 1_000_000)) |val| {
        return val;
    } else |e| {
        try Sexp.warn("Error", heap, run.err);
        return e;
    }
}

pub fn expectEvalHeap(heap: *Heap, want: []const u8, src: []const u8) !void {
    if (evalString(heap, src)) |val| {
        const valueString = try Sexp.printAlloc(heap.orb, heap, val);

        defer heap.orb.free(valueString);

        const wantValue = try Sexp.read(heap, want);
        const wantString = try Sexp.printAlloc(heap.orb, heap, wantValue);

        defer heap.orb.free(wantString);

        try expectEqualStrings(wantString, valueString);
    } else |e| {
        return e;
    }
}

pub fn expectEval(want: []const u8, src: []const u8) !void {
    var heap = try newTestHeap();
    defer heap.deinit();

    try expectEvalHeap(&heap, want, src);
}

test "(+ 1 2 3) => 6" {
    try expectEval("6", "(+ 1 2 3)");
}

test "(+ (+ 1 2) (+ 3 4))" {
    try expectEval("10", "(+ (+ 1 2) (+ 3 4))");
}

test "(head (cons 1 2)) => 1" {
    try expectEval("1", "(head (cons 1 2))");
}

test "(tail (cons 1 2)) => 2" {
    try expectEval("2", "(tail (cons 1 2))");
}

test "nil => nil" {
    try expectEval("nil", "nil");
}

test "if" {
    try expectEval("0", "(if nil 1 0)");
    try expectEval("1", "(if t 1 0)");
}

test "do" {
    try expectEval("3", "(do 1 2 3)");
}

test "returning" {
    try expectEval("1", "(returning 1 2 3)");
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
        \\ (do
        \\   (let ((ten 10))
        \\     (set-symbol-function! 'foo (fn (x y) (+ ten x y))))
        \\   (foo 1 2))
    );
}

test "calling a macro closure" {
    try expectEval("3",
        \\ (do
        \\   (set-symbol-function! 'frob
        \\      (%macro-fn (x y z)
        \\        (list y x z)))
        \\   (frob 1 + 2))
    );
}

test "(list 1 2 3)" {
    try expectEval("(1 2 3)", "(list 1 2 3)");
}

test "EQ?" {
    try expectEval("T", "(eq? 1 1)");
    try expectEval("NIL", "(eq? 1 2)");
    try expectEval("T", "(eq? 'foo 'foo)");
    try expectEval("NIL", "(eq? 'foo 'bar)");
}

test "DEFUN" {
    try expectEval("(1 . 2)",
        \\ (do (defun f (x y) (cons x y)) (f 1 2))
    );
}

test "base test suite" {
    var heap = try newTestHeap();
    defer heap.deinit();

    try heap.cookTest();
    try expectEvalHeap(&heap, "nil", "(base-test)");
}

test "FUNCALL" {
    try expectEval("(b . a)",
        \\ (call (fn (x y) (cons y x)) 'a 'b)
    );
}

test "APPLY" {
    try expectEval("(a b c)",
        \\ (apply (fn (x y z) (list x y z)) '(a b c))
    );
}

test "defun with &rest" {
    try expectEval("(x . (1 2 3))",
        \\ (do
        \\   (defun foo (x &rest xs) (cons x xs))
        \\   (foo 'x 1 2 3))
    );
}

test "defmacro with &rest" {
    try expectEval("(1 2 3)",
        \\ (do
        \\   (defmacro foo (x &rest xs) (cons x xs))
        \\   (foo list 1 2 3))
    );
}

test "MAP with FN" {
    try expectEval("(2 3 4)",
        \\ (map (fn (x) (+ x 1)) '(1 2 3))
    );
}

test "GENKEY!" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const x = try evalString(&heap, "(genkey!)");
    const y = try evalString(&heap, "(genkey!)");

    try std.testing.expect(x != y);
}

test "CALL-WITH-PROMPT" {
    var heap = try newTestHeap();
    defer heap.deinit();

    const x = try evalString(&heap,
        \\(call-with-prompt 'foo
        \\ (fn () 1)
        \\ (fn (v k)
        \\   k))
    );

    try std.testing.expectEqual(x, 1);
}

test "SYMBOL-NAME" {
    try expectEval(
        \\("FOO" "NIL" "T")
    ,
        \\(map #'symbol-name '(foo nil t))
    );
}

test "STRING-SEARCH" {
    try expectEval(
        \\3
    ,
        \\(string-search "FOOBAR!" "BAR")
    );
}

test "STRING-SLICE" {
    try expectEval(
        \\"BAR"
    ,
        \\(string-slice "FOOBAR!" 3 (+ 3 3))
    );
}
