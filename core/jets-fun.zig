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

const Disk = @import("./disk.zig");
const File = @import("./file.zig");
const Jets = @import("./jets.zig");
const Sexp = @import("./sexp.zig");
const Step = @import("./step.zig");
const Tape = @import("./tape.zig");

const Heap = Wisp.Heap;
const Rest = Jets.Rest;

const t = Wisp.t;
const nil = Wisp.nil;
const nah = Wisp.nah;
const top = Wisp.top;
const tagOf = Wisp.tagOf;

fn wordint(x: u32) !i31 {
    if (tagOf(x) != .int) return Wisp.Oof.Err;
    return @bitCast(i31, @intCast(u31, x));
}

fn intword(x: i31) u32 {
    return @intCast(u32, @bitCast(u31, x));
}

pub fn @"RETURNING"(step: *Step, xs: []u32) anyerror!void {
    _ = step;
    step.give(.val, xs[0]);
}

pub fn @"+"(step: *Step, xs: []u32) anyerror!void {
    _ = step;

    var result: i31 = 0;
    for (xs) |x| {
        if (@addWithOverflow(i31, result, try wordint(x), &result)) {
            return step.fail(&[_]u32{step.heap.kwd.@"FIXNUM-OVERFLOW"});
        }
    }

    step.give(.val, intword(result));
}

pub fn @"*"(step: *Step, xs: []u32) anyerror!void {
    _ = step;

    var result: i31 = 1;
    for (xs) |x| {
        if (@mulWithOverflow(i31, result, try wordint(x), &result)) {
            return step.fail(&[_]u32{step.heap.kwd.@"FIXNUM-OVERFLOW"});
        }
    }

    step.give(.val, intword(result));
}

pub fn @"/"(step: *Step, xs: []u32) anyerror!void {
    if (xs.len == 1) {
        return @"/"(step, &[_]u32{ 1, xs[0] });
    } else {
        var result: i31 = try wordint(xs[0]);
        for (xs[1..xs.len]) |x| {
            result = std.math.divFloor(i31, result, try wordint(x)) catch {
                return step.fail(&[_]u32{
                    step.heap.kwd.@"BAD-FIXNUM-DIVISION",
                    intword(result),
                    x,
                });
            };
        }

        step.give(.val, intword(result));
    }
}

pub fn @"-"(step: *Step, a: u32, xs: []u32) anyerror!void {
    _ = step;

    var result: i31 = try wordint(a);
    for (xs) |x| {
        if (@subWithOverflow(i31, result, try wordint(x), &result)) {
            return step.fail(&[_]u32{step.heap.kwd.@"FIXNUM-OVERFLOW"});
        }
    }

    step.give(.val, intword(result));
}

pub fn @"CONS"(step: *Step, car: u32, cdr: u32) anyerror!void {
    step.give(.val, try step.heap.cons(car, cdr));
}

pub fn @"HEAD"(step: *Step, x: u32) anyerror!void {
    if (x == nil) {
        step.give(.val, nil);
    } else if (step.heap.get(.duo, .car, x)) |car| {
        step.give(.val, car);
    } else |_| {
        try step.fail(&[_]u32{
            step.heap.kwd.@"TYPE-MISMATCH",
            step.heap.kwd.@"CONS",
            x,
        });
    }
}

pub fn @"TAIL"(step: *Step, x: u32) anyerror!void {
    if (x == nil) {
        step.give(.val, nil);
    } else if (step.heap.get(.duo, .cdr, x)) |cdr| {
        step.give(.val, cdr);
    } else |_| {
        try step.fail(&[_]u32{
            step.heap.kwd.@"TYPE-MISMATCH",
            step.heap.kwd.@"CONS",
            x,
        });
    }
}

pub fn @"SYMBOL-FUNCTION"(step: *Step, sym: u32) anyerror!void {
    step.give(.val, try step.heap.get(.sym, .fun, sym));
}

pub fn @"SET-SYMBOL-FUNCTION!"(
    step: *Step,
    sym: u32,
    fun: u32,
) anyerror!void {
    try step.heap.set(.sym, .fun, sym, fun);

    if (tagOf(fun) == .fun) {
        try step.heap.set(.fun, .sym, fun, sym);
    } else if (tagOf(fun) == .mac) {
        try step.heap.set(.mac, .sym, fun, sym);
    }

    step.give(.val, fun);
}

pub fn @"SET-SYMBOL-VALUE!"(
    step: *Step,
    sym: u32,
    val: u32,
) anyerror!void {
    try step.heap.set(.sym, .val, sym, val);
    step.give(.val, val);
}

pub fn @"LIST"(step: *Step, xs: []u32) anyerror!void {
    var cur = nil;
    var i = xs.len;

    while (i > 0) : (i -= 1) {
        cur = try step.heap.cons(xs[i - 1], cur);
    }

    step.give(.val, cur);
}

pub fn @"EQ?"(step: *Step, x: u32, y: u32) anyerror!void {
    step.give(.val, if (x == y) t else nil);
}

pub fn @"PRINT"(step: *Step, x: u32) anyerror!void {
    const stdout = std.io.getStdOut().writer();
    const pretty = try Sexp.prettyPrint(step.heap, x, 78);
    defer step.heap.orb.free(pretty);
    try stdout.print("{s}\n", .{pretty});
    step.give(.val, x);
}

pub fn @"TYPE-OF"(step: *Step, x: u32) anyerror!void {
    const kwd = step.heap.kwd;

    if (x == nil) {
        step.give(.val, kwd.NULL);
    } else if (x == t) {
        step.give(.val, kwd.BOOLEAN);
    } else {
        step.give(.val, switch (tagOf(x)) {
            .int => kwd.INTEGER,
            .chr => kwd.CHARACTER,
            .duo => kwd.CONS,
            .sym => kwd.SYMBOL,
            .fun => kwd.FUNCTION,
            .mac => kwd.MACRO,
            .jet => kwd.FUNCTION,
            .v32 => kwd.VECTOR,
            .v08 => kwd.STRING,
            .pkg => kwd.PACKAGE,
            .ktx => kwd.CONTINUATION,
            .run => kwd.EVALUATOR,
            .sys => unreachable,
        });
    }
}

pub fn @"ERROR"(step: *Step, xs: []u32) anyerror!void {
    try step.fail(xs);
}

pub fn @"GET/CC"(step: *Step) anyerror!void {
    step.give(.val, step.run.way);
}

pub fn SAVE(step: *Step) anyerror!void {
    const key = try step.heap.genkey();
    const name = try step.heap.symstrslice(key);

    step.give(.val, key);
    try Tape.save(step.heap, name);
}

pub fn CALL(
    step: *Step,
    function: u32,
    arguments: Rest,
) anyerror!void {
    try step.call(
        function,
        arguments.arg,
        false,
    );
}

pub fn APPLY(
    step: *Step,
    function: u32,
    list: u32,
) anyerror!void {
    try step.call(function, list, false);
}

pub fn @"CALL/CC"(step: *Step, function: u32) anyerror!void {
    try step.call(
        function,
        try step.heap.cons(step.run.way, nil),
        true,
    );
}

pub fn WTF(step: *Step, wtf: u32) anyerror!void {
    Step.wtf = wtf != nil;
    step.give(.val, wtf);
}

pub fn CONCATENATE(step: *Step, typ: u32, rest: Rest) anyerror!void {
    _ = rest;
    if (typ == step.heap.kwd.STRING) {
        try step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"});
    } else {
        try step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"});
    }
}

pub fn LOAD(step: *Step, src: u32) anyerror!void {
    const path = try step.heap.v08slice(src);

    const code = try Disk.readFileAlloc(step.heap.orb, path);
    defer step.heap.orb.free(code);

    const forms = try Sexp.readMany(step.heap, code);
    defer forms.deinit();

    for (forms.items) |form| {
        var run = Step.initRun(form);
        try Sexp.warn("loading", step.heap, form);
        if (Step.evaluate(step.heap, &run, 1_000)) |_| {} else |err| {
            try Sexp.warn("failed", step.heap, form);
            try Sexp.warn("condition", step.heap, run.err);
            step.run.err = run.err;
            return err;
        }
    }

    step.give(.val, t);
}

pub fn ENV(step: *Step) anyerror!void {
    step.give(.val, step.run.env);
}

pub fn RUN(step: *Step, exp: u32) anyerror!void {
    const run = Wisp.Row(.run){
        .way = top,
        .env = step.run.env,
        .err = nil,
        .val = nah,
        .exp = exp,
    };

    step.give(.val, try step.heap.new(.run, run));
}

pub fn @"STEP!"(step: *Step, runptr: u32) anyerror!void {
    var run = try step.heap.row(.run, runptr);
    try Step.once(step.heap, &run);
    try step.heap.put(.run, runptr, run);
    step.give(.val, nil);
}

pub fn CODE(step: *Step, fun: u32) anyerror!void {
    return switch (tagOf(fun)) {
        .fun => step.give(.val, try step.heap.get(.fun, .exp, fun)),
        .mac => step.give(.val, try step.heap.get(.mac, .exp, fun)),
        .jet => step.give(.val, nil),
        else => step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"}),
    };
}

pub fn @"SET-CODE!"(step: *Step, fun: u32, exp: u32) anyerror!void {
    switch (tagOf(fun)) {
        .fun => try step.heap.set(.fun, .exp, fun, exp),
        .mac => try step.heap.set(.mac, .exp, fun, exp),
        else => try step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"}),
    }

    step.give(.val, fun);
}

pub fn AREF(step: *Step, vec: u32, idx: u32) anyerror!void {
    switch (tagOf(vec)) {
        .v32 => {
            const xs = try step.heap.v32slice(vec);
            if (tagOf(idx) == .int and idx < xs.len) {
                step.give(.val, xs[idx]);
            } else {
                try step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"});
            }
        },

        else => try step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"}),
    }
}

pub fn REQUEST(step: *Step, req: u32, rest: Rest) anyerror!void {
    try step.fail(&[_]u32{ step.heap.kwd.@"REQUEST", req, rest.arg });
}

pub fn @"GENKEY!"(step: *Step) anyerror!void {
    step.give(.val, try step.heap.genkey());
}

pub fn @"KEY?"(step: *Step, val: u32) anyerror!void {
    switch (tagOf(val)) {
        .sym => {
            const pkg = try step.heap.get(.sym, .pkg, val);
            step.give(
                .val,
                if (pkg == step.heap.keyPackage) t else nil,
            );
        },

        else => step.give(.val, nil),
    }
}

pub fn @"PROGNIFY"(step: *Step, arg: u32) anyerror!void {
    // () => ()
    // (foo bar) => (progn foo bar)
    // (foo) => foo
    // ((foo)) => (foo)
    if ((try Wisp.length(step.heap, arg)) > 1) {
        step.give(.val, try step.heap.cons(step.heap.kwd.PROGN, arg));
    } else if (arg == nil) {
        step.give(.val, nil);
    } else {
        step.give(.val, try step.heap.get(.duo, .car, arg));
    }
}

/// This is a continuation control operator.  It invokes a thunk
/// in the context of a new prompt with the given tag and
/// handler function.
pub fn @"CALL-WITH-PROMPT"(
    step: *Step,
    TAG: u32,
    THUNK: u32,
    HANDLER: u32,
) anyerror!void {
    const ktx = try step.heap.new(.ktx, .{
        .hop = step.run.way,
        .env = step.run.env,
        .fun = step.heap.kwd.PROMPT,
        .acc = TAG,
        .arg = HANDLER,
    });

    step.run.way = ktx;

    try step.call(THUNK, nil, false);
}

/// This is a continuation control operator.
pub fn @"SEND!"(
    step: *Step,
    TAG: u32,
    VALUE: u32,
) anyerror!void {
    // We search the current context for a prompt that matches
    // the tag.  If we don't find it, that's an error.
    //
    // As we're searching, we're copying the context, ending
    // with TOP when we get to the matching tag.
    //
    // Then we invoke the handler function with the given value
    // and the delimited continuation we've created.
    //
    //   𝐸₁[call-with-prompt 𝐸₂[send 𝑣] 𝑒] ⟶ 𝐸₁[𝑒 𝑣 (λ𝑥. 𝐸₂[𝑥])]
    //

    if (try copyContinuationSlice(
        step.heap,
        step.run.way,
        TAG,
    )) |result| {
        const args = try Wisp.list(
            step.heap,
            &[_]u32{ VALUE, result.e2 },
        );

        if (result.e1 == top) {
            step.run.way = top;
            step.run.env = nil;
        } else {
            step.run.way = result.e1;
        }

        return step.call(result.handler, args, false);
    } else {
        return step.fail(&[_]u32{
            step.heap.kwd.@"PROMPT-TAG-MISSING",
            TAG,
        });
    }
}

fn isMatchingPrompt(
    heap: *Heap,
    ktx: u32,
    tag: u32,
) !bool {
    if (ktx == top) return false;
    return tag == try heap.get(.ktx, .acc, ktx);
}

fn copyContinuationSlice(
    heap: *Heap,
    ktx: u32,
    tag: u32,
) !?ContinuationCopyResult {
    if (try isMatchingPrompt(heap, ktx, tag)) {
        return ContinuationCopyResult{
            .handler = try heap.get(.ktx, .arg, ktx),
            .e1 = try heap.get(.ktx, .hop, ktx),
            .e2 = top,
        };
    }

    var new = try heap.copy(.ktx, ktx);
    var cur = new;

    while (true) {
        const hop = try heap.get(.ktx, .hop, cur);
        if (try isMatchingPrompt(heap, hop, tag)) {
            try heap.set(.ktx, .hop, cur, top);
            return ContinuationCopyResult{
                .handler = try heap.get(.ktx, .arg, hop),
                .e1 = try heap.get(.ktx, .hop, hop),
                .e2 = new,
            };
        } else {
            cur = hop;
        }
    }

    return null;
}

const ContinuationCopyResult = struct {
    handler: u32,
    e1: u32,
    e2: u32,
};

pub fn @"%SET!"(step: *Step, sym: u32, val: u32) anyerror!void {
    var cur = step.run.env;
    while (cur != nil) {
        var curduo = try step.heap.row(.duo, cur);
        var v32 = try step.heap.v32slice(curduo.car);
        var i: usize = 0;
        while (i < v32.len) : (i += 2) {
            if (v32[i] == sym) {
                v32[i + 1] = val;
                return step.give(.val, val);
            }
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
            return Wisp.Oof.Err;
        },
        else => |_| {
            try step.heap.set(.sym, .val, sym, val);
            step.give(.val, val);
        },
    }
}

pub fn @"KTX-HOP"(step: *Step, ktx: u32) anyerror!void {
    const hop = try step.heap.get(.ktx, .hop, ktx);
    step.give(.val, hop);
}

pub fn @"KTX-FUN"(step: *Step, ktx: u32) anyerror!void {
    const fun = try step.heap.get(.ktx, .fun, ktx);
    step.give(.val, fun);
}

pub fn @"KTX-ENV"(step: *Step, ktx: u32) anyerror!void {
    const env = try step.heap.get(.ktx, .env, ktx);
    step.give(.val, env);
}

pub fn @"KTX-ACC"(step: *Step, ktx: u32) anyerror!void {
    const acc = try step.heap.get(.ktx, .acc, ktx);
    step.give(.val, acc);
}

pub fn @"KTX-ARG"(step: *Step, ktx: u32) anyerror!void {
    const arg = try step.heap.get(.ktx, .arg, ktx);
    step.give(.val, arg);
}

pub fn @"TOP?"(step: *Step, ktx: u32) anyerror!void {
    step.give(.val, if (ktx == top) t else nil);
}

pub fn @"READ-LINE"(step: *Step) anyerror!void {
    const stdin = std.io.getStdIn().reader();

    if (try File.readLine(step.heap.orb, stdin)) |line| {
        defer step.heap.orb.free(line);
        step.give(.val, try step.heap.newv08(line));
    } else {
        step.give(.val, nil);
    }
}

pub fn @"READ"(step: *Step, src: u32) anyerror!void {
    const bytes = try step.heap.v08slice(src);
    const val = try Sexp.read(step.heap, bytes);
    step.give(.val, val);
}

pub fn @"WRITE"(step: *Step, v08: u32) anyerror!void {
    const bytes = try step.heap.v08slice(v08);
    try std.io.getStdOut().writeAll(bytes);
    step.give(.val, nil);
}

pub fn @"EVAL"(step: *Step, exp: u32) anyerror!void {
    step.run.exp = exp;
    step.run.val = nah;
}

pub fn @"COMPOSE-CONTINUATION"(step: *Step, ktx: u32) anyerror!void {
    step.give(.val, try step.composeContinuation(ktx));
}

pub fn @"RUN-WAY"(step: *Step, run: u32) anyerror!void {
    step.give(.val, try step.heap.get(.run, .way, run));
}

pub fn @"RUN-EXP"(step: *Step, run: u32) anyerror!void {
    const row = try step.heap.row(.run, run);

    const duo = if (row.exp == nah)
        try step.heap.cons(step.heap.kwd.@"VAL", row.val)
    else
        try step.heap.cons(step.heap.kwd.@"EXP", row.exp);

    step.give(.val, duo);
}

pub fn @"RUN-VAL"(step: *Step, run: u32) anyerror!void {
    step.give(.val, try step.heap.get(.run, .val, run));
}

pub fn @"RUN-ERR"(step: *Step, run: u32) anyerror!void {
    step.give(.val, try step.heap.get(.run, .err, run));
}

pub fn @"MACROEXPAND-1"(step: *Step, code: u32) anyerror!void {
    if (Wisp.tagOf(code) == .duo) {
        const duo = try step.heap.row(.duo, code);
        if (Wisp.tagOf(duo.car) == .sym) {
            const fun = try step.heap.get(.sym, .fun, duo.car);
            if (Wisp.tagOf(fun) == .mac) {
                try step.call(fun, duo.cdr, false);
                return;
            }
        }
    }

    step.give(.val, code);
}

pub fn @"PACKAGE-SYMBOLS"(step: *Step, pkg: u32) anyerror!void {
    step.give(.val, try step.heap.get(.pkg, .sym, pkg));
}

pub fn @"FIND-PACKAGE"(step: *Step, name: u32) anyerror!void {
    const bytes = try step.heap.v08slice(name);
    if (step.heap.pkgmap.get(bytes)) |pkg| {
        step.give(.val, pkg);
    } else {
        step.give(.val, nil);
    }
}

pub fn @"JET?"(step: *Step, fun: u32) anyerror!void {
    step.give(.val, if (Wisp.tagOf(fun) == .jet) t else nil);
}
