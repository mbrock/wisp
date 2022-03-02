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

const Job = Eval;

fn int(x: u32) !i31 {
    if (wisp.tagOf(x) != .int) return wisp.Oof.Err;
    return @intCast(i31, x);
}

pub fn @"PROG1"(job: *Job, xs: []u32) anyerror!u32 {
    _ = job;
    return xs[0];
}

pub fn @"+"(job: *Job, xs: []u32) anyerror!u32 {
    _ = job;

    var result: i31 = 0;
    for (xs) |x| {
        result += try int(x);
    }

    return @intCast(u32, result);
}

pub fn @"CONS"(job: *Job, car: u32, cdr: u32) anyerror!u32 {
    return job.ctx.new(.duo, .{ .car = car, .cdr = cdr });
}

pub fn @"CAR"(job: *Job, x: u32) anyerror!u32 {
    return job.ctx.get(.duo, .car, x);
}

pub fn @"CDR"(job: *Job, x: u32) anyerror!u32 {
    return job.ctx.get(.duo, .cdr, x);
}

pub fn @"SET-FUNCTION"(job: *Job, sym: u32, fun: u32) anyerror!u32 {
    try job.ctx.set(.sym, .fun, sym, fun);
    return fun;
}

pub fn @"LIST"(job: *Job, xs: []u32) anyerror!u32 {
    var cur = wisp.nil;
    var i = xs.len;
    while (i > 0) : (i -= 1) {
        cur = try job.ctx.new(.duo, .{ .car = xs[i - 1], .cdr = cur });
    }
    return cur;
}

pub fn @"EQ"(job: *Job, x: u32, y: u32) anyerror!u32 {
    _ = job;
    return if (x == y) wisp.t else wisp.nil;
}

pub fn @"PRINT"(job: *Job, x: u32) anyerror!u32 {
    const out = std.io.getStdOut().writer();
    try dump.dump(job.ctx, out, x);
    try out.writeByte('\n');
    return x;
}

pub fn @"TYPE-OF"(job: *Job, x: u32) anyerror!u32 {
    const kwd = job.ctx.kwd;

    if (x == wisp.nil) return kwd.NULL;
    if (x == wisp.t) return kwd.BOOLEAN;

    return switch (wisp.tagOf(x)) {
        .int => kwd.INTEGER,
        .chr => kwd.CHARACTER,
        .duo => kwd.CONS,
        .sym => kwd.SYMBOL,
        .fun, .fop => kwd.FUNCTION,
        .mac, .mop => kwd.MACRO,
        .v32 => kwd.VECTOR,
        .v08 => kwd.STRING,
        .pkg => kwd.PACKAGE,

        .ct0,
        .ct1,
        .ct2,
        .ct3,
        => kwd.CONTINUATION,

        .sys => unreachable,
    };
}

pub fn @"ERROR"(job: *Job, xs: []u32) anyerror!u32 {
    const out = std.io.getStdOut().writer();
    try out.print("ERROR: ", .{});
    for (xs) |x| {
        try dump.dump(job.ctx, out, x);
        try out.writeByte(' ');
    }
    try out.writeByte('\n');
    return wisp.Oof.Err;
}

pub fn @"GET/CC"(job: *Job) anyerror!u32 {
    return job.way;
}

pub fn SAVE(job: *Job, @"CORE-NAME": u32) anyerror!u32 {
    return try wisp.core.save(job, try job.ctx.v08slice(@"CORE-NAME"));
}
