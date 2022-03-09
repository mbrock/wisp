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

const Step = @import("./step.zig");
const Sexp = @import("./sexp.zig");
const Disk = @import("./disk.zig");
const Jets = @import("./jets.zig");

const Rest = Jets.Rest;

fn int(x: u32) !i31 {
    if (Wisp.tagOf(x) != .int) return Wisp.Oof.Err;
    return @intCast(i31, x);
}

pub fn @"PROG1"(step: *Step, xs: []u32) anyerror!void {
    _ = step;
    step.give(.val, xs[0]);
}

pub fn @"+"(step: *Step, xs: []u32) anyerror!void {
    _ = step;

    var result: i31 = 0;
    for (xs) |x| {
        result += try int(x);
    }

    step.give(.val, @intCast(u32, result));
}

pub fn @"CONS"(step: *Step, car: u32, cdr: u32) anyerror!void {
    step.give(.val, try step.heap.new(.duo, .{ .car = car, .cdr = cdr }));
}

pub fn @"CAR"(step: *Step, x: u32) anyerror!void {
    if (x == Wisp.nil) {
        step.give(.val, Wisp.nil);
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

pub fn @"CDR"(step: *Step, x: u32) anyerror!void {
    if (x == Wisp.nil) {
        step.give(.val, Wisp.nil);
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

pub fn @"SET-SYMBOL-FUNCTION"(
    step: *Step,
    sym: u32,
    fun: u32,
) anyerror!void {
    try step.heap.set(.sym, .fun, sym, fun);
    step.give(.val, fun);
}

pub fn @"SET-SYMBOL-VALUE"(
    step: *Step,
    sym: u32,
    val: u32,
) anyerror!void {
    try step.heap.set(.sym, .val, sym, val);
    step.give(.val, val);
}

pub fn @"LIST"(step: *Step, xs: []u32) anyerror!void {
    var cur = Wisp.nil;
    var i = xs.len;

    while (i > 0) : (i -= 1) {
        cur = try step.heap.new(.duo, .{ .car = xs[i - 1], .cdr = cur });
    }

    step.give(.val, cur);
}

pub fn @"EQ"(step: *Step, x: u32, y: u32) anyerror!void {
    step.give(.val, if (x == y) Wisp.t else Wisp.nil);
}

pub fn @"PRINT"(step: *Step, x: u32) anyerror!void {
    const out = std.io.getStdOut().writer();
    try Sexp.dump(step.heap, out, x);
    try out.writeByte('\n');
    step.give(.val, x);
}

pub fn @"TYPE-OF"(step: *Step, x: u32) anyerror!void {
    const kwd = step.heap.kwd;

    if (x == Wisp.nil) {
        step.give(.val, kwd.NULL);
    } else if (x == Wisp.t) {
        step.give(.val, kwd.BOOLEAN);
    } else {
        step.give(.val, switch (Wisp.tagOf(x)) {
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

pub fn SAVE(step: *Step, @"CORE-NAME": u32) anyerror!void {
    step.give(
        .val,
        try Disk.save(
            step,
            try step.heap.v08slice(@"CORE-NAME"),
        ),
    );
}

pub fn FUNCALL(
    step: *Step,
    function: u32,
    arguments: Rest,
) anyerror!void {
    try step.call(
        step.run.way,
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
    try step.call(step.run.way, function, list, false);
}

pub fn @"CALL/CC"(step: *Step, function: u32) anyerror!void {
    // Take the parent continuation of the CALL/CC form.
    const hop = try step.heap.get(.ktx, .hop, step.run.way);
    try step.call(step.run.way, function, try step.heap.new(.duo, .{
        .car = hop,
        .cdr = Wisp.nil,
    }), true);
}

pub fn WTF(step: *Step, wtf: u32) anyerror!void {
    Step.wtf = wtf > 0;
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
        if (Step.evaluate(step.heap, &run, 1_000, false)) |_| {} else |err| {
            try Sexp.warn("failed", step.heap, form);
            try Sexp.warn("condition", step.heap, run.err);
            step.run.err = run.err;
            return err;
        }
    }

    step.give(.val, Wisp.t);
}

pub fn ENV(step: *Step) anyerror!void {
    step.give(.val, step.run.env);
}

pub fn RUN(step: *Step, exp: u32) anyerror!void {
    step.give(.val, try step.heap.new(.run, Step.initRun(exp)));
}

pub fn STEP(step: *Step, runptr: u32) anyerror!void {
    var run = try step.heap.row(.run, runptr);
    try Step.once(step.heap, &run);
    try step.heap.put(.run, runptr, run);
    step.give(.val, Wisp.nil);
}

pub fn CODE(step: *Step, fun: u32) anyerror!void {
    return switch (Wisp.tagOf(fun)) {
        .fun => step.give(.val, try step.heap.get(.fun, .exp, fun)),
        .mac => step.give(.val, try step.heap.get(.mac, .exp, fun)),
        else => step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"}),
    };
}

pub fn AREF(step: *Step, vec: u32, idx: u32) anyerror!void {
    switch (Wisp.tagOf(vec)) {
        .v32 => {
            const xs = try step.heap.v32slice(vec);
            if (Wisp.tagOf(idx) == .int and idx < xs.len) {
                step.give(.val, xs[idx]);
            } else {
                try step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"});
            }
        },

        else => try step.fail(&[_]u32{step.heap.kwd.@"PROGRAM-ERROR"}),
    }
}
