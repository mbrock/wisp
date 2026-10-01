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
const Tape = @import("./tape.zig");
const Profile = @import("./profile.zig");
const Continuation = @import("./continuation.zig");

const Step = @This();
const Heap = Wisp.Heap;

pub const Run = Wisp.Row(.run);

const Status = enum { val, exp };

heap: *Heap,
run: *Run,
tmp: std.mem.Allocator,

pub var wtf = false;

pub fn initRun(exp: u32) Run {
    return .{
        .way = top,
        .meta = top,
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
    var tmp = std.heap.stackFallback(4096, heap.orb);
    var step = Step{
        .heap = heap,
        .run = run,
        .tmp = tmp.get(),
    };
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
    const pkg = try step.heap.get(.sym, .pkg, sym);
    if (pkg == step.heap.keywordPackage) {
        step.run.exp = nah;
        step.run.val = sym;
        return;
    }

    const dyn = try step.heap.get(.sym, .dyn, sym);
    if (dyn != nil) {
        if (try step.findDynamicBinding(sym)) |ktx| {
            step.give(.val, try Continuation.value(step.heap, ktx));
            return;
        }

        // If dynamic variable has no dynamic binding, treat it
        // as a lexical variable.
    }

    var frames: u32 = 0;
    var comparisons: u32 = 0;
    var cur = step.run.env;
    while (cur != nil) {
        if (comptime Profile.enabled) frames += 1;
        const curduo = try step.heap.row(.duo, cur);
        const v32 = try step.heap.v32slice(curduo.car);
        var i: usize = 0;
        while (i < v32.len) : (i += 2) {
            if (comptime Profile.enabled) comparisons += 1;
            if (v32[i] == sym) {
                Profile.recordLexicalLookup(
                    frames,
                    comparisons,
                    false,
                );
                return step.give(.val, v32[i + 1]);
            }
        }
        cur = curduo.cdr;
    }

    Profile.recordLexicalLookup(frames, comparisons, true);
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
    return Continuation.findBinding(step.heap, step.run.meta, name);
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
    // Argument vectors stay writable until capture freezes their
    // frame. A resumed frame gets a private vector on first write.
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
        if (step.run.meta == top) {
            step.run.env = nil;
            step.give(.val, x);
        } else {
            const entry = try step.heap.row(.ktx, step.run.meta);
            (try Continuation.outer(step.heap, entry)).install(step.run);
            step.run.env = entry.env;
        }
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
    var vals = try step.scanListAlloc(arg);
    defer vals.deinit(step.tmp);

    if (rev)
        Profile.recordListReverse(vals.items.len);
    if (rev)
        std.mem.reverse(u32, vals.items);

    try step.scanValues(fun, exp, par, vals.items);
}

fn scanValues(
    step: *Step,
    fun: u32,
    exp: u32,
    par: u32,
    vals: []const u32,
) !void {
    var pars = try step.scanListAlloc(par);
    defer pars.deinit(step.tmp);

    Profile.recordCallArity(vals.len);
    var scope = try step.tmp.alloc(u32, 2 * pars.items.len);
    defer step.tmp.free(scope);

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
                vals[m..vals.len],
            );

            n += 1;
            m = vals.len;

            break :loop;
        } else if (x == step.heap.kwd.@"&OPTIONAL") {
            optional = true;
        } else if (m < vals.len) {
            scope[n * 2 + 0] = x;
            scope[n * 2 + 1] = vals[m];
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
                @intCast(vals.len),
                fun,
            });
        }
    }

    if (m < vals.len) {
        try step.fail(&[_]u32{
            step.heap.kwd.@"PROGRAM-ERROR",
            step.heap.kwd.@"INVALID-ARGUMENT-COUNT",
            @intCast(vals.len),
            fun,
        });
    }

    step.run.env = try step.heap.cons(
        try step.heap.newv32(scope[0 .. n * 2]),
        step.run.env,
    );

    step.give(.exp, exp);
}

fn symname(step: *Step, sym: u32) ![]const u8 {
    if (tagOf(sym) == .sym) {
        return try step.heap.v08slice(try step.heap.get(.sym, .str, sym));
    } else {
        return "anonymous";
    }
}

/// Perform an application, either by directly calling a builtin
/// or by entering a closure.
pub fn call(
    step: *Step,
    funptr: u32,
    args: u32,
    rev: bool,
) anyerror!void {
    if (wtf) {
        try step.warn("call", funptr);
    }

    switch (tagOf(funptr)) {
        .jet => {
            try step.oper(funptr, args, rev);
        },

        .fun => {
            Profile.recordCall(.fun);
            const fun = try step.heap.row(.fun, funptr);
            step.run.env = fun.env;
            try step.scan(funptr, fun.exp, fun.par, args, rev);
            try step.heap.set(.fun, .cnt, funptr, 1 + fun.cnt);
        },

        .mac => {
            Profile.recordCall(.mac);
            const mac = try step.heap.row(.mac, funptr);
            step.run.env = mac.env;
            try step.scan(funptr, mac.exp, mac.par, args, rev);
            try step.heap.set(.mac, .cnt, funptr, 1 + mac.cnt);
        },

        .ktx => {
            Profile.recordCall(.continuation);
            var vals = try step.scanListAlloc(args);
            defer vals.deinit(step.tmp);

            if (vals.items.len != 1) {
                try step.fail(&[_]u32{
                    step.heap.kwd.@"PROGRAM-ERROR",
                    step.heap.kwd.@"CONTINUATION-CALL-ERROR",
                });
            } else {
                (try step.composeContinuation(funptr)).install(step.run);
                step.give(.val, vals.items[0]);
                try step.proceed(vals.items[0]);
            }
        },

        .sys => {
            if (funptr == top) {
                Profile.recordCall(.continuation);
                var vals = try step.scanListAlloc(args);
                defer vals.deinit(step.tmp);

                if (vals.items.len != 1) {
                    try step.fail(&.{
                        step.heap.kwd.@"PROGRAM-ERROR",
                        step.heap.kwd.@"CONTINUATION-CALL-ERROR",
                    });
                } else {
                    (try step.composeContinuation(funptr)).install(step.run);
                    step.give(.val, vals.items[0]);
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

fn callValues(step: *Step, funptr: u32, args: []u32) !void {
    switch (tagOf(funptr)) {
        .jet => try step.operValues(funptr, args),

        .fun => {
            Profile.recordCall(.fun);
            const fun = try step.heap.row(.fun, funptr);
            step.run.env = fun.env;
            try step.scanValues(funptr, fun.exp, fun.par, args);
            try step.heap.set(.fun, .cnt, funptr, 1 + fun.cnt);
        },

        else => return error.BadFunctionTag,
    }
}

pub fn warn(step: *Step, text: []const u8, exp: u32) !void {
    try Sexp.warn(text, step.heap, exp);
}

pub fn composeContinuation(step: *Step, way: u32) !Continuation.Context {
    return Continuation.compose(step.heap, try Continuation.context(step.heap, way), step.run.*);
}

pub fn debug(heap: *Heap, txt: []const u8, val: u32) !void {
    try Sexp.warn(txt, heap, val);
}

fn writableFrame(step: *Step, frame: Row(.ktx)) !Row(.ktx) {
    if (ref(step.run.way) >= step.heap.frozen_ktx) return frame;
    step.run.way = try step.heap.copyContinuationFrame(step.run.way);
    return step.heap.row(.ktx, step.run.way);
}

const Ktx = struct {
    fn funargs(step: *Step, original: Row(.ktx)) !void {
        Profile.recordArgument();
        const ktx = if (original.acc == nil and original.arg == nil)
            original
        else
            try step.writableFrame(original);

        // Come back to the environment of the call form.
        step.run.env = ktx.env;

        if (ktx.acc == nil and ktx.arg == nil) {
            var value = [1]u32{step.run.val};
            step.run.way = ktx.hop;
            try step.callValues(ktx.fun, &value);
        } else if (ktx.acc == nil) {
            const vector = try step.heap.filledv32(
                2 + try Wisp.length(step.heap, ktx.arg),
                nil,
            );
            var acc = try step.heap.v32slice(vector);
            acc[0] = 1;
            acc[1] = step.run.val;
            const argduo = try step.heap.row(.duo, ktx.arg);
            try step.heap.set(.ktx, .acc, step.run.way, vector);
            try step.heap.set(.ktx, .arg, step.run.way, argduo.cdr);
            step.give(.exp, argduo.car);
        } else {
            var acc = try step.heap.v32slice(ktx.acc);
            const pos = acc[0];
            if (pos + 1 >= acc.len)
                return error.BadContinuationArgumentIndex;
            acc[pos + 1] = step.run.val;
            acc[0] = pos + 1;
            if (ktx.arg == nil) {
                step.run.way = ktx.hop;
                try step.callValues(ktx.fun, acc[1..]);
            } else {
                const argduo = try step.heap.row(.duo, ktx.arg);
                try step.heap.set(.ktx, .arg, step.run.way, argduo.cdr);
                step.give(.exp, argduo.car);
            }
        }
    }

    fn EVAL(step: *Step, ktx: Row(.ktx)) !void {
        const exp = step.run.val;
        step.run.* = .{
            .err = nil,
            .env = ktx.env,
            .way = ktx.hop,
            .meta = step.run.meta,
            .exp = exp,
            .val = nah,
        };
    }

    fn DO(step: *Step, ktx: Row(.ktx)) !void {
        if (ktx.arg == nil) {
            step.run.way = ktx.hop;
            step.run.env = ktx.env;
        } else {
            const argduo = try step.heap.row(.duo, ktx.arg);

            step.give(.exp, argduo.car);
            step.run.env = ktx.env;

            if (argduo.cdr == nil) {
                step.run.way = ktx.hop;
            } else {
                _ = try step.writableFrame(ktx);
                try step.heap.set(.ktx, .arg, step.run.way, argduo.cdr);
            }
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

            _ = try step.writableFrame(ktx);
            try step.heap.set(.ktx, .acc, step.run.way, symacc);
            try step.heap.set(.ktx, .arg, step.run.way, argduo.cdr);

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

    var scope: std.ArrayList(u32) = .empty;
    defer scope.deinit(heap.orb);

    const accduo = try heap.row(.duo, acc);
    const letsym = accduo.car;

    try scope.append(heap.orb, letsym);
    try scope.append(heap.orb, val);

    {
        var curduo = try heap.row(.duo, accduo.cdr);
        while (curduo.cdr != nil) {
            const cdrduo = try heap.row(.duo, curduo.cdr);
            const curval = curduo.car;
            const cursym = cdrduo.car;

            try scope.append(heap.orb, cursym);
            try scope.append(heap.orb, curval);

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

    pub fn arrayList(this: *List) *std.ArrayList(u32) {
        return switch (this.*) {
            .proper => |*xs| xs,
            .dotted => |*xs| xs,
        };
    }

    pub fn deinit(this: *List, allocator: std.mem.Allocator) void {
        switch (this.*) {
            .proper => |*xs| xs.deinit(allocator),
            .dotted => |*xs| xs.deinit(allocator),
        }
    }
};

pub fn scanListAlloc(step: *Step, list: u32) !std.ArrayList(u32) {
    return switch (try scanListAllocAllowDotted(step.heap, step.tmp, list)) {
        .proper => |xs| xs,
        .dotted => Oof.Err,
    };
}

pub fn scanListAllocAllowDotted(heap: *Heap, tmp: Wisp.Orb, list: u32) !List {
    var xs = try std.ArrayList(u32).initCapacity(tmp, 64);
    errdefer xs.deinit(tmp);

    var cur = list;
    var cells: usize = 0;
    while (tagOf(cur) == .duo) {
        const duo = try heap.row(.duo, cur);
        try xs.append(tmp, duo.car);
        cur = duo.cdr;
        if (comptime Profile.enabled) cells += 1;
    }
    Profile.recordListScan(cells);

    if (cur == nil) {
        return List{ .proper = xs };
    } else {
        try xs.append(tmp, cur);
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

    const slice = buffer[0..i];
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

fn operValues(step: *Step, jet: u32, args: []u32) !void {
    if (step.invokeJetValues(jet, args)) {
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
    var list = try step.scanListAlloc(arg);
    defer list.deinit(step.tmp);
    if (rev) {
        Profile.recordListReverse(list.items.len);
        std.mem.reverse(u32, list.items);
    }
    try step.invokeJetValues(jet, list.items);
}

fn invokeJetValues(step: *Step, jet: u32, args: []u32) !void {
    const def = Jets.jets[Wisp.Imm.from(jet).idx];
    Profile.recordCall(.jet);
    Profile.recordCallArity(args.len);

    switch (def.tag) {
        .f0x => {
            // Slice-taking jets may allocate another heap vector,
            // relocating the backing store of the argument state.
            const stable = try step.tmp.dupe(u32, args);
            defer step.tmp.free(stable);
            const fun = cast(.f0x, def);
            try fun(step, stable);
        },

        .f0r => {
            const fun = cast(.f0r, def);
            try fun(step, .{ .arg = try Wisp.list(step.heap, args) });
        },

        .f1r => {
            if (args.len < 1) return step.invalidArgumentCount(jet);
            const fun = cast(.f1r, def);
            try fun(
                step,
                args[0],
                .{ .arg = try Wisp.list(step.heap, args[1..]) },
            );
        },

        .f1x => {
            if (args.len < 1) return step.invalidArgumentCount(jet);
            const stable = try step.tmp.dupe(u32, args);
            defer step.tmp.free(stable);
            const fun = cast(.f1x, def);
            try fun(step, stable[0], stable[1..]);
        },

        .f2x => {
            if (args.len < 2) {
                return step.invalidArgumentCount(jet);
            } else {
                const stable = try step.tmp.dupe(u32, args);
                defer step.tmp.free(stable);
                const fun = cast(.f2x, def);
                try fun(
                    step,
                    stable[0],
                    stable[1],
                    stable[2..],
                );
            }
        },

        .f0 => {
            if (args.len == 0) {
                const fun = cast(.f0, def);
                try fun(step);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f1 => {
            if (args.len == 1) {
                const fun = cast(.f1, def);
                try fun(step, args[0]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f2 => {
            if (args.len == 2) {
                const fun = cast(.f2, def);
                try fun(step, args[0], args[1]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f3 => {
            if (args.len == 3) {
                const fun = cast(.f3, def);
                try fun(step, args[0], args[1], args[2]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        .f4 => {
            if (args.len == 4) {
                const fun = cast(.f4, def);
                try fun(step, args[0], args[1], args[2], args[3]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },

        // XXX: I know this is horrible.  I'm going to refactor it later...
        .f5 => {
            if (args.len == 5) {
                const fun = cast(.f5, def);
                try fun(step, args[0], args[1], args[2], args[3], args[4]);
            } else {
                try step.invalidArgumentCount(jet);
            }
        },
    }
}

pub fn stepOver(heap: *Heap, run: *Run, limit: u32) !void {
    const breakpoint = try Continuation.snapshot(heap, Continuation.Context.fromRun(run.*));
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
        .ktx => (try Continuation.view(heap, way)).hop,
        .duo => heap.get(.duo, .cdr, way),
        else => error.BadParentContinuation,
    };
}

pub fn stepOut(heap: *Heap, run: *Run, limit: u32) !void {
    const ctx = try Continuation.snapshot(heap, Continuation.Context.fromRun(run.*));
    const breakpoint = if (ctx == top) top else try getParentContinuation(heap, ctx);
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

    var stop = try Continuation.context(heap, breakpoint);
    try heap.roots.append(heap.orb, &stop.way);
    defer _ = heap.roots.pop();
    try heap.roots.append(heap.orb, &stop.meta);
    defer _ = heap.roots.pop();

    var tmp = std.heap.stackFallback(4096, heap.orb);

    var step = Step{
        .heap = heap,
        .run = run,
        .tmp = tmp.get(),
    };

    var i: u32 = 0;
    while (true) {
        if (limit > 0 and i >= limit) break;

        if (!heap.inhibit_gc and (heap.please_tidy or (limit == 0 and i > 0 and @mod(i, 100_000) == 0))) {
            // var timer = try std.time.Timer.start();
            // const s0 = heap.bytesize();

            const gc_started = if (comptime Profile.enabled)
                Profile.beginGc(heap.cap, heap.bytesize())
            else
                0;
            defer {
                if (comptime Profile.enabled) Profile.leaveGc();
            }

            var gc = try prepareToTidy(&step);
            try finishTidying(&step, &gc);
            if (comptime Profile.enabled) {
                Profile.finishGc(
                    heap.cap,
                    gc_started,
                    heap.bytesize(),
                );
            }

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

        if (run.val != nah and ((run.way == top and run.meta == top) or
            try Continuation.same(heap, Continuation.Context.fromRun(run.*), stop)))
        {
            return run.val;
        }

        Profile.evaluatorStep();
        if (step.attemptOneStep() catch |e| step.handleError(e)) {
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
    try gc.move(&step.run.meta);
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
    return try Heap.fromEmbeddedCore(std.testing.allocator, std.testing.io);
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

test "captured argument vectors are independent continuation snapshots" {
    try expectEval(
        \\(pause (before one after) (before two after))
    ,
        \\(do
        \\  (defvar *saved-argument-continuation* nil)
        \\  (let
        \\    ((initial
        \\       (call-with-prompt 'save-arguments
        \\         (fn ()
        \\           (list 'before
        \\                 (send! 'save-arguments 'pause)
        \\                 'after))
        \\         (fn (value continuation)
        \\           (do
        \\             (set! *saved-argument-continuation*
        \\                   continuation)
        \\             value)))))
        \\    (list initial
        \\          (call *saved-argument-continuation* 'one)
        \\          (call *saved-argument-continuation* 'two))))
    );
}

test "CALL-WITH-EFFECT-HANDLER reinstalls its prompt" {
    try expectEval(
        \\50
    ,
        \\(call-with-effect-handler 'ask
        \\  (fn () (+ (send! 'ask 2)
        \\            (send! 'ask 3)))
        \\  (fn (request resume raise)
        \\    (call resume (* request 10))))
    );
}

test "CALL-WITH-EFFECT-HANDLER resumes after its first evaluation" {
    var heap = try newTestHeap();
    defer heap.deinit();

    _ = try evalString(&heap,
        \\(do
        \\  (defvar *saved-widget-pin* nil)
        \\  (defparameter *widget-resume* nil))
    );
    const first = try evalString(&heap,
        \\(call-with-effect-handler 'widget
        \\  (fn () (do (send! 'widget 'first)
        \\             (send! 'widget 'second)))
        \\  (fn (view resume raise)
        \\    (binding ((*widget-resume* resume))
        \\      (let ((local-resume *widget-resume*))
        \\        (set! *saved-widget-pin*
        \\              (make-pinned-value
        \\               (fn (event)
        \\                 (call local-resume event))))))
        \\    view))
    );
    const first_string = try Sexp.printAlloc(heap.orb, &heap, first);
    defer heap.orb.free(first_string);
    try expectEqualStrings("FIRST", first_string);
    _ = try evalString(&heap, "(gc)");

    const pin = try evalString(&heap, "*saved-widget-pin*");
    const fun = heap.pins.get(Wisp.Imm.from(pin).idx).?;
    const again = try evalString(&heap, "'again");
    const args = try heap.cons(again, nil);
    var run = initRun(nil);
    var tmp = std.heap.stackFallback(4096, heap.orb);
    var step = Step{
        .heap = &heap,
        .run = &run,
        .tmp = tmp.get(),
    };
    try step.call(fun, args, false);
    const second = try evaluate(&heap, &run, 1_000_000);
    const second_string = try Sexp.printAlloc(heap.orb, &heap, second);
    defer heap.orb.free(second_string);
    try expectEqualStrings("SECOND", second_string);
}

test "CALL-WITH-EFFECT-HANDLER raises into the suspended continuation" {
    try expectEval(
        \\(caught nope)
    ,
        \\(try
        \\  (call-with-effect-handler 'ask
        \\    (fn () (send! 'ask nil))
        \\    (fn (request resume raise)
        \\      (call raise 'nope)))
        \\  (catch (error restart)
        \\    (list 'caught (head error))))
    );
}

test "standard output is a dynamically bound effect" {
    try expectEval(
        \\"hello WORLD\n"
    ,
        \\(call-with-effect-handler 'capture
        \\  (fn ()
        \\    (binding ((*standard-output* 'capture))
        \\      (write "hello ")
        \\      (print 'world)
        \\      ""))
        \\  (fn (request resume raise)
        \\    (string-append
        \\     (apply #'string-append (tail request))
        \\     (call resume nil))))
    );
}

test "standard input is a dynamically bound effect" {
    try expectEval(
        \\42
    ,
        \\(call-with-effect-handler 'answer
        \\  (fn ()
        \\    (binding ((*standard-input* 'answer))
        \\      (+ (read) (read))))
        \\  (fn (request resume raise)
        \\    (call resume (list 21))))
    );
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

test "preexpansion walks callback bodies and preserves function metadata" {
    try expectEval(
        \\(future-callee (%fn nil (x &optional y &rest zs) (if x (list y zs) nil)))
    ,
        \\(macroexpand-completely
        \\ '(future-callee (fn (x &optional y &rest zs) (when x (list y zs)))))
    );
    try expectEval("(%fn named (x) (if x x nil))",
        \\(macroexpand-completely '(%fn named (x) (when x x)))
    );
    try expectEval("(%macro-fn (x) (if x x nil))",
        \\(macroexpand-completely '(%macro-fn (x) (when x x)))
    );
    try expectEval("(let ((x (if t 7 nil))) (%fn nil (y) (+ x y)))",
        \\(macroexpand-completely '(let ((x (when t 7))) (fn (y) (+ x y))))
    );
}

test "preexpansion preserves quoted data and unchanged form identity" {
    try expectEval("t",
        \\(let ((form '(list '(fn (when) (when x)) (function when)
        \\                   (%fn named (when) when))))
        \\  (eq? form (macroexpand-completely form)))
    );
    try expectEval("(append (list (if t 7 nil)) (quote nil))",
        \\(macroexpand-completely '(backquote ((unquote (when t 7)))))
    );
}

test "preexpansion makes standalone lambdas eager and keeps lexical scope" {
    try expectEval("((11 12) t)",
        \\(do
        \\  (defmacro %test-add (x) (list '+ x 10))
        \\  (let ((f (let ((offset 1))
        \\             (fn (x) (%test-add (+ offset x))))))
        \\    (let ((before (function-call-count #'%test-add)))
        \\      (list (list (call f 0) (call f 1))
        \\            (eq? before (function-call-count #'%test-add))))))
    );
}

test "preexpansion supports terminating recursive macro helpers" {
    try expectEval("(1 2 3 4)",
        \\(do
        \\  (defun %test-expand-list (xs)
        \\    (if (nil? xs) nil
        \\      (list 'cons (head xs) (%test-expand-list (tail xs)))))
        \\  (defmacro %test-list (&rest xs) (%test-expand-list xs))
        \\  (call (fn () (%test-list 1 2 3 4))))
    );
    try expectEval("(cons 1 (cons 2 (cons 3 nil)))",
        \\(do
        \\  (defmacro %test-recursive-list (&rest xs)
        \\    (if (nil? xs) nil
        \\      (list 'cons (head xs) (cons '%test-recursive-list (tail xs)))))
        \\  (macroexpand-completely '(%test-recursive-list 1 2 3)))
    );
}

test "preexpansion bounds branching recursive macros and leaves runtime forms" {
    try expectEval("(7 t nil)",
        \\(do
        \\  (defmacro %test-branch (x)
        \\    (list 'if x
        \\      (list 'list (list '%test-branch x) (list '%test-branch x)) 7))
        \\  (let ((before (function-call-count #'%test-branch)))
        \\    (let ((f (call-with-binding '*macroexpand-limit* 8
        \\               (%fn nil () (fn (x) (%test-branch x))))))
        \\      (list (call f nil)
        \\            (eq? 8 (- (function-call-count #'%test-branch) before))
        \\            *macroexpand-budget*))))
    );
}

test "preexpansion shares its budget with reentrant eager lambda expansion" {
    try expectEval("(t nil)",
        \\(do
        \\  (defmacro %test-nested-fn () (list 'fn nil (list '%test-nested-fn)))
        \\  (let ((before (function-call-count #'%test-nested-fn)))
        \\    (call-with-binding '*macroexpand-limit* 8
        \\      (%fn nil () (macroexpand-completely '(%test-nested-fn))))
        \\    (list (< (- (function-call-count #'%test-nested-fn) before) 9)
        \\          *macroexpand-budget*)))
    );
}

test "preexpansion bounds self-reproducing macros and can be disabled" {
    try expectEval("((%test-self 1) t)",
        \\(do
        \\  (defmacro %test-self (&rest args) (cons '%test-self args))
        \\  (list
        \\    (call-with-binding '*macroexpand-limit* 4
        \\      (%fn nil () (macroexpand-completely '(%test-self 1))))
        \\    (let ((form '(when t 1)))
        \\      (call-with-binding '*macroexpand-limit* 0
        \\        (%fn nil () (eq? form (macroexpand-completely form)))))))
    );
}

test "preexpansion budget unwinds on nonlocal exit" {
    try expectEval("(17 nil)",
        \\(do
        \\  (defmacro %test-expansion-exit () (send! 'expand-exit 17))
        \\  (list
        \\    (call-with-prompt 'expand-exit
        \\      (fn () (macroexpand-completely '(%test-expansion-exit)))
        \\      (fn (value continuation) value))
        \\    *macroexpand-budget*))
    );
}

test "effect sentinels do not intern symbols and preserve fallback values" {
    var heap = try newTestHeap();
    defer heap.deinit();
    _ = try evalString(&heap,
        \\(defun %test-sentinels (n)
        \\  (if (eq? n 0) nil
        \\    (do
        \\      (send-or-invoke 'missing 1 (fn (v) v))
        \\      (send-to-or-invoke (get/cc) 'missing 2 (fn (v) v))
        \\      (call-with-prompt 'found
        \\        (fn () (send! 'found 3))
        \\        (fn (v k) (do (gc) v)))
        \\      (%test-sentinels (- n 1)))))
    );
    const before = try Wisp.length(&heap, try heap.get(.pkg, .sym, heap.keyPackage));
    try expectEvalHeap(&heap, "nil", "(%test-sentinels 100)");
    _ = try evalString(&heap, "(gc)");
    try expectEqual(before, try Wisp.length(&heap, try heap.get(.pkg, .sym, heap.keyPackage)));
    try expectEvalHeap(&heap, "(nil (1 . 2))",
        \\(list
        \\  (send-or-invoke 'missing nil (fn (v) (do (gc) v)))
        \\  (send-to-or-invoke (get/cc) 'missing (cons 1 2)
        \\    (fn (v) (do (gc) v))))
    );
}

test "preexpansion eliminates runtime router macro expansion" {
    var heap = try newTestHeap();
    defer heap.deinit();
    _ = try heap.load(@embedFile("lisp/repo-benchmarks.wisp"));
    try expectEvalHeap(&heap, "((\"alice\") not-found t t t)",
        \\(let ((handles (function-call-count #'handle))
        \\      (lambdas (function-call-count #'fn))
        \\      (backquotes (function-call-count #'backquote)))
        \\  (let ((hit (%benchmark-router-hit 100))
        \\        (miss (%benchmark-router-miss 100)))
        \\    (list hit miss
        \\      (eq? handles (function-call-count #'handle))
        \\      (eq? lambdas (function-call-count #'fn))
        \\      (eq? backquotes (function-call-count #'backquote)))))
    );
}

test "segmented prompts cross inner prompts and select the nearest matching tag" {
    try expectEval("(116 1113)",
        \\(list
        \\  (call-with-prompt 'outer
        \\    (fn ()
        \\      (+ 100 (call-with-prompt 'inner
        \\               (fn () (+ 10 (send! 'outer 3)))
        \\               (fn (v k) (+ 1000 (call k v))))))
        \\    (fn (v k) (call k (* v 2))))
        \\  (call-with-prompt 'same
        \\    (fn ()
        \\      (+ 100 (call-with-prompt 'same
        \\               (fn () (+ 10 (send! 'same 3)))
        \\               (fn (v k) (+ 1000 (call k v))))))
        \\    (fn (v k) 9999)))
    );
}

test "segmented LET and DO snapshots share lexical store but not control progress" {
    try expectEval("(pause (1 7 3 1) (1 9 3 2) 2)",
        \\(let ((saved nil) (store 0))
        \\  (let ((initial
        \\          (call-with-prompt 'save
        \\            (fn ()
        \\              (let ((a 1) (b (send! 'save 'pause)) (c 3))
        \\                (set! store (+ store 1))
        \\                (list a b c store)))
        \\            (fn (v k) (do (set! saved k) v)))))
        \\    (list initial (call saved 7) (call saved 9) store)))
    );
    try expectEval("(pause 7 8 2)",
        \\(let ((saved nil) (store 0))
        \\  (let ((initial
        \\          (call-with-prompt 'save
        \\            (fn ()
        \\              (do (send! 'save 'pause)
        \\                  (set! store (+ store 1))
        \\                  (+ store 6)))
        \\            (fn (v k) (do (set! saved k) v)))))
        \\    (list initial (call saved nil) (call saved nil) store)))
    );
}

test "segmented dynamic bindings snapshot captured values and update caller bindings" {
    try expectEval("(100 (11 20) 11 100)",
        \\(do
        \\  (defparameter *snapshot-binding* 100)
        \\  (let ((saved nil))
        \\    (let ((initial
        \\            (call-with-prompt 'save
        \\              (fn ()
        \\                (binding ((*snapshot-binding* 10))
        \\                  (send! 'save nil)
        \\                  (set! *snapshot-binding* (+ *snapshot-binding* 1))
        \\                  *snapshot-binding*))
        \\              (fn (v k) (do (set! saved k) *snapshot-binding*)))))
        \\      (list initial
        \\        (binding ((*snapshot-binding* 20))
        \\          (list (call saved nil) *snapshot-binding*))
        \\        (call saved nil) *snapshot-binding*))))
    );
    try expectEval("(pause (21 21) (31 31) 100)",
        \\(do
        \\  (defparameter *caller-binding* 100)
        \\  (let ((saved nil))
        \\    (let ((initial
        \\            (call-with-prompt 'save
        \\              (fn ()
        \\                (send! 'save 'pause)
        \\                (set! *caller-binding* (+ *caller-binding* 1))
        \\                *caller-binding*)
        \\              (fn (v k) (do (set! saved k) v)))))
        \\      (list initial
        \\        (binding ((*caller-binding* 20))
        \\          (list (call saved nil) *caller-binding*))
        \\        (binding ((*caller-binding* 30))
        \\          (list (call saved nil) *caller-binding*))
        \\        *caller-binding*))))
    );
}

test "segmented SEND-TO composes the suspended outer context without consuming it" {
    try expectEval("(missing 3117 112)",
        \\(let ((saved
        \\        (call-with-prompt 'park
        \\          (fn ()
        \\            (+ 100 (call-with-prompt 'fault
        \\                     (fn () (+ 10 (send! 'park nil)))
        \\                     (fn (v k) (+ 1000 (call k v))))))
        \\          (fn (v k) k))))
        \\  (list
        \\    (send-to-with-default! saved 'absent 1 'missing)
        \\    (+ 2000 (send-to-with-default! saved 'fault 7 'missing))
        \\    (apply saved '(2))))
    );
}

test "segmented capture preserves a resuming caller across another capture" {
    try expectEval("1107",
        \\(let ((first
        \\        (call-with-prompt 'park
        \\          (fn () (do (send! 'park nil) (send! 'outer nil)))
        \\          (fn (v k) k))))
        \\  (let ((second
        \\          (call-with-prompt 'outer
        \\            (fn () (+ 100 (call first nil)))
        \\            (fn (v k) k))))
        \\    (+ 1000 (call second 7))))
    );
}

test "segmented GET/CC is an immutable snapshot" {
    var heap = try newTestHeap();
    defer heap.deinit();
    _ = try evalString(&heap, "(defvar *getcc-snapshot* nil)");
    try expectEvalHeap(&heap, "7",
        \\(call-with-prompt 'park
        \\  (fn ()
        \\    (set! *getcc-snapshot* (get/cc))
        \\    (gc)
        \\    (send! 'park 7)
        \\    999)
        \\  (fn (v k) v))
    );
    var saved = try evalString(&heap, "*getcc-snapshot*");
    try heap.roots.append(heap.orb, &saved);
    defer _ = heap.roots.pop();
    for (0..2) |_| {
        var run = initRun(nil);
        var tmp = std.heap.stackFallback(4096, heap.orb);
        var step = Step{ .heap = &heap, .run = &run, .tmp = tmp.get() };
        try step.call(saved, try heap.cons(nil, nil), false);
        try expectEqual(@as(u32, 7), try evaluate(&heap, &run, 10_000));
    }
}

test "segmented images preserve captured contexts and suspended run meta" {
    var heap = try newTestHeap();
    defer heap.deinit();
    try expectEvalHeap(&heap, "parked",
        \\(do
        \\  (defparameter *image-binding* 5)
        \\  (defvar *image-continuation* nil)
        \\  (call-with-prompt 'park
        \\    (fn ()
        \\      (binding ((*image-binding* 10))
        \\        (call-with-prompt 'inner
        \\          (fn () (+ *image-binding* (send! 'park nil)))
        \\          (fn (v k) 999))))
        \\    (fn (v k) (do (set! *image-continuation* k) 'parked))))
    );
    var run = initRun(try Sexp.read(&heap,
        \\(call-with-prompt 'done
        \\  (%fn nil ()
        \\    (call-with-binding '*image-binding* 20
        \\      (%fn nil () (+ 1 2 3))))
        \\  (%fn nil (v k) 999))
    ));
    var steps: usize = 0;
    while (!(run.exp == 2 and run.way != top and run.meta != top)) : (steps += 1) {
        try std.testing.expect(steps < 1000);
        try once(&heap, &run);
    }
    const pin = try heap.newPin(try Continuation.snapshot(&heap, Continuation.Context.fromRun(run)));
    var runptr = try heap.new(.run, run);
    var roots = [_]*u32{&runptr};
    try Tidy.gc(&heap, &roots);
    const bytes = try heap.orb.alloc(u8, Tape.byteSize(&heap));
    defer heap.orb.free(bytes);
    _ = try Tape.writeToMemory(&heap, bytes);
    var clone = try Tape.loadFromMemory(std.testing.allocator, std.testing.io, bytes);
    defer clone.deinit();
    var restored = try clone.row(.run, runptr);
    const k = clone.pins.get(Wisp.Imm.from(pin).idx).?;
    try expectEqual(@as(u32, 6), try evaluate(&clone, &restored, 1000));
    try expectEqual(top, restored.meta);
    const args = try Continuation.view(&clone, k);
    try expectEqual(@as(u32, 1), (try clone.v32slice(args.acc))[0]);
    const binding = try Continuation.view(&clone, args.hop);
    try expectEqual(clone.kwd.BINDING, binding.fun);
    try expectEqual(@as(u32, 20), binding.arg);
    const prompt = try Continuation.view(&clone, binding.hop);
    try expectEqual(clone.kwd.PROMPT, prompt.fun);
    try expectEqual(top, prompt.hop);
    for ([_]u32{ 7, 9 }, [_]u32{ 11, 13 }) |input, expected| {
        var resumed = initRun(nil);
        var tmp = std.heap.stackFallback(4096, clone.orb);
        var step = Step{ .heap = &clone, .run = &resumed, .tmp = tmp.get() };
        try step.call(k, try clone.cons(input, nil), false);
        try expectEqual(expected, try evaluate(&clone, &resumed, 1000));
    }
    try expectEvalHeap(&clone, "(17 19 5)",
        \\(list (call *image-continuation* 7)
        \\      (call *image-continuation* 9) *image-binding*)
    );
}

test "segmented debugger stepping survives binding updates and collection" {
    var heap = try newTestHeap();
    defer heap.deinit();
    _ = try evalString(&heap, "(defparameter *step-binding* 0)");
    var run = initRun(try Sexp.read(&heap,
        \\(binding ((*step-binding* 10))
        \\  (+ 1 (do (set! *step-binding* 11) (gc) 2) 3))
    ));
    const plus = try heap.get(.sym, .fun, try heap.intern("+", heap.base));
    var steps: usize = 0;
    while (!(tagOf(run.exp) == .duo and
        try heap.get(.duo, .car, run.exp) == heap.kwd.DO and
        run.way != top and run.meta != top and
        try heap.get(.ktx, .fun, run.way) == plus)) : (steps += 1)
    {
        try std.testing.expect(steps < 10_000);
        try once(&heap, &run);
    }
    try stepOver(&heap, &run, 10_000);
    try expectEqual(@as(u32, 3), run.exp);
    const binding = (try Continuation.findBinding(&heap, run.meta, try heap.intern("*STEP-BINDING*", heap.base))).?;
    try expectEqual(@as(u32, 11), try Continuation.value(&heap, binding));
    try stepOut(&heap, &run, 10_000);
    try expectEqual(@as(u32, 6), run.val);
    try expectEqual(heap.kwd.DO, try heap.get(.ktx, .fun, run.way));
    try stepOut(&heap, &run, 10_000);
    try expectEqual(top, run.way);
    try std.testing.expect(run.meta != top);
    try stepOut(&heap, &run, 10_000);
    try expectEqual(top, run.meta);
    try expectEqual(@as(u32, 6), run.val);
}
