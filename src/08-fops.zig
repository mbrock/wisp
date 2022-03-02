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

const std = @import("std");

const wisp = @import("./ff-wisp.zig");

const Eval = @import("./04-eval.zig");
const dump = @import("./06-dump.zig");
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
    job.give(.val, try job.ctx.new(.duo, .{ .car = car, .cdr = cdr }));
}

pub fn @"CAR"(job: *Eval, x: u32) anyerror!void {
    job.give(.val, try job.ctx.get(.duo, .car, x));
}

pub fn @"CDR"(job: *Eval, x: u32) anyerror!void {
    job.give(.val, try job.ctx.get(.duo, .cdr, x));
}

pub fn @"SET-FUNCTION"(job: *Eval, sym: u32, fun: u32) anyerror!void {
    try job.ctx.set(.sym, .fun, sym, fun);
    job.give(.val, fun);
}

pub fn @"LIST"(job: *Eval, xs: []u32) anyerror!void {
    var cur = wisp.nil;
    var i = xs.len;

    while (i > 0) : (i -= 1) {
        cur = try job.ctx.new(.duo, .{ .car = xs[i - 1], .cdr = cur });
    }

    job.give(.val, cur);
}

pub fn @"EQ"(job: *Eval, x: u32, y: u32) anyerror!void {
    job.give(.val, if (x == y) wisp.t else wisp.nil);
}

pub fn @"PRINT"(job: *Eval, x: u32) anyerror!void {
    const out = std.io.getStdOut().writer();
    try dump.dump(job.ctx, out, x);
    try out.writeByte('\n');
    job.give(.val, x);
}

pub fn @"TYPE-OF"(job: *Eval, x: u32) anyerror!void {
    const kwd = job.ctx.kwd;

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
            .fun, .fop => kwd.FUNCTION,
            .mac, .mop => kwd.MACRO,
            .v32 => kwd.VECTOR,
            .v08 => kwd.STRING,
            .pkg => kwd.PACKAGE,
            .ct0, .ct1, .ct2, .ct3 => kwd.CONTINUATION,
            .sys => unreachable,
        });
    }
}

pub fn @"ERROR"(job: *Eval, xs: []u32) anyerror!void {
    const out = std.io.getStdOut().writer();
    try out.print("ERROR: ", .{});
    for (xs) |x| {
        try dump.dump(job.ctx, out, x);
        try out.writeByte(' ');
    }
    try out.writeByte('\n');
    return wisp.Oof.Err;
}

pub fn @"GET/CC"(job: *Eval) anyerror!void {
    job.give(.val, job.way);
}

pub fn SAVE(job: *Eval, @"CORE-NAME": u32) anyerror!void {
    job.give(.val, try wisp.core.save(job, try job.ctx.v08slice(@"CORE-NAME")));
}

pub fn FUNCALL(
    job: *Eval,
    FUNCTION: u32,
    ARGUMENTS: Rest,
) anyerror!void {
    try job.apply(job.way, FUNCTION, ARGUMENTS.arg, true);
}
