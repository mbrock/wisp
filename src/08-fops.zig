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

const wisp = @import("./ff-wisp.zig");

const Eval = @import("./04-eval.zig");
const read = @import("./05-read.zig");
const dump = @import("./06-dump.zig");
const disk = @import("./0b-disk.zig");
const Rest = @import("./07-xops.zig").Rest;

fn int(x: u32) !i31 {
    if (wisp.tagOf(x) != .int) return wisp.Oof.Err;
    return @intCast(i31, x);
}

pub fn @"PROG1"(job: *Eval, xs: []u32) anyerror!void {
    _ = job;
    job.give(.val, xs[0]);
}

pub fn @"+"(job: *Eval, xs: []u32) anyerror!void {
    _ = job;

    var result: i31 = 0;
    for (xs) |x| {
        result += try int(x);
    }

    job.give(.val, @intCast(u32, result));
}

pub fn @"CONS"(job: *Eval, car: u32, cdr: u32) anyerror!void {
    job.give(.val, try job.heap.new(.duo, .{ .car = car, .cdr = cdr }));
}

pub fn @"CAR"(job: *Eval, x: u32) anyerror!void {
    if (x == wisp.nil) {
        job.give(.val, wisp.nil);
    } else if (job.heap.get(.duo, .car, x)) |car| {
        job.give(.val, car);
    } else |_| {
        try job.fail(&[_]u32{
            job.heap.kwd.@"TYPE-MISMATCH",
            job.heap.kwd.@"CONS",
            x,
        });
    }
}

pub fn @"CDR"(job: *Eval, x: u32) anyerror!void {
    if (x == wisp.nil) {
        job.give(.val, wisp.nil);
    } else if (job.heap.get(.duo, .cdr, x)) |cdr| {
        job.give(.val, cdr);
    } else |_| {
        try job.fail(&[_]u32{
            job.heap.kwd.@"TYPE-MISMATCH",
            job.heap.kwd.@"CONS",
            x,
        });
    }
}

pub fn @"SET-SYMBOL-FUNCTION"(
    job: *Eval,
    sym: u32,
    fun: u32,
) anyerror!void {
    try job.heap.set(.sym, .fun, sym, fun);
    job.give(.val, fun);
}

pub fn @"SET-SYMBOL-VALUE"(
    job: *Eval,
    sym: u32,
    val: u32,
) anyerror!void {
    try job.heap.set(.sym, .val, sym, val);
    job.give(.val, val);
}

pub fn @"LIST"(job: *Eval, xs: []u32) anyerror!void {
    var cur = wisp.nil;
    var i = xs.len;

    while (i > 0) : (i -= 1) {
        cur = try job.heap.new(.duo, .{ .car = xs[i - 1], .cdr = cur });
    }

    job.give(.val, cur);
}

pub fn @"EQ"(job: *Eval, x: u32, y: u32) anyerror!void {
    job.give(.val, if (x == y) wisp.t else wisp.nil);
}

pub fn @"PRINT"(job: *Eval, x: u32) anyerror!void {
    const out = std.io.getStdOut().writer();
    try dump.dump(job.heap, out, x);
    try out.writeByte('\n');
    job.give(.val, x);
}

pub fn @"TYPE-OF"(job: *Eval, x: u32) anyerror!void {
    const kwd = job.heap.kwd;

    if (x == wisp.nil) {
        job.give(.val, kwd.NULL);
    } else if (x == wisp.t) {
        job.give(.val, kwd.BOOLEAN);
    } else {
        job.give(.val, switch (wisp.tagOf(x)) {
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
            .bot => kwd.EVALUATOR,
            .sys => unreachable,
        });
    }
}

pub fn @"ERROR"(job: *Eval, xs: []u32) anyerror!void {
    try job.fail(xs);
}

pub fn @"GET/CC"(job: *Eval) anyerror!void {
    job.give(.val, job.bot.way);
}

pub fn SAVE(job: *Eval, @"CORE-NAME": u32) anyerror!void {
    job.give(
        .val,
        try wisp.core.save(
            job,
            try job.heap.v08slice(@"CORE-NAME"),
        ),
    );
}

pub fn FUNCALL(
    job: *Eval,
    function: u32,
    arguments: Rest,
) anyerror!void {
    try job.call(
        job.bot.way,
        function,
        arguments.arg,
        false,
    );
}

pub fn APPLY(
    job: *Eval,
    function: u32,
    list: u32,
) anyerror!void {
    try job.call(job.bot.way, function, list, false);
}

pub fn @"CALL/CC"(job: *Eval, function: u32) anyerror!void {
    // Take the parent continuation of the CALL/CC form.
    const hop = try job.heap.get(.ktx, .hop, job.bot.way);
    try job.call(job.bot.way, function, try job.heap.new(.duo, .{
        .car = hop,
        .cdr = wisp.nil,
    }), true);
}

pub fn WTF(job: *Eval, wtf: u32) anyerror!void {
    Eval.wtf = wtf > 0;
    job.give(.val, wtf);
}

pub fn CONCATENATE(job: *Eval, typ: u32, rest: Rest) anyerror!void {
    _ = rest;
    if (typ == job.heap.kwd.STRING) {
        try job.fail(&[_]u32{job.heap.kwd.@"PROGRAM-ERROR"});
    } else {
        try job.fail(&[_]u32{job.heap.kwd.@"PROGRAM-ERROR"});
    }
}

pub fn LOAD(job: *Eval, src: u32) anyerror!void {
    const path = try job.heap.v08slice(src);

    const code = try disk.readFileAlloc(job.heap.orb, path);
    defer job.heap.orb.free(code);

    const forms = try read.readMany(job.heap, code);
    defer forms.deinit();

    for (forms.items) |form| {
        var bot = Eval.initBot(form);
        var exe = Eval.init(job.heap, &bot);
        try dump.warn("loading", job.heap, form);
        if (exe.evaluate(1_000, false)) |_| {} else |err| {
            try dump.warn("failed", job.heap, form);
            try dump.warn("condition", job.heap, exe.bot.err);
            job.bot.err = exe.bot.err;
            return err;
        }
    }

    job.give(.val, wisp.t);
}

pub fn ENV(job: *Eval) anyerror!void {
    job.give(.val, job.bot.env);
}

pub fn BOT(job: *Eval, exp: u32) anyerror!void {
    job.give(.val, try job.heap.new(.bot, Eval.initBot(exp)));
}

pub fn STEP(job: *Eval, botptr: u32) anyerror!void {
    var bot = try job.heap.row(.bot, botptr);
    var exe = Eval.init(job.heap, &bot);

    try exe.step();
    try job.heap.put(.bot, botptr, bot);
    job.give(.val, wisp.nil);
}
