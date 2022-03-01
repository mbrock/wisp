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

const Ctx = Eval;

const Err = error{Error};

fn int(x: u32) !i31 {
    if (wisp.tagOf(x) != .int) return Err.Error;
    return @intCast(i31, x);
}

pub fn @"PROG1"(ctx: *Ctx, xs: []u32) anyerror!u32 {
    _ = ctx;
    return xs[0];
}

pub fn @"+"(ctx: *Ctx, xs: []u32) anyerror!u32 {
    _ = ctx;

    var result: i31 = 0;
    for (xs) |x| {
        result += try int(x);
    }

    return @intCast(u32, result);
}

pub fn @"CONS"(ctx: *Ctx, car: u32, cdr: u32) anyerror!u32 {
    return ctx.ctx.new(.duo, .{ .car = car, .cdr = cdr });
}

pub fn @"CAR"(ctx: *Ctx, x: u32) anyerror!u32 {
    return ctx.ctx.get(.duo, .car, x);
}

pub fn @"CDR"(ctx: *Ctx, x: u32) anyerror!u32 {
    return ctx.ctx.get(.duo, .cdr, x);
}

pub fn @"SET-FUNCTION"(ctx: *Ctx, sym: u32, fun: u32) anyerror!u32 {
    try ctx.ctx.set(.sym, .fun, sym, fun);
    return fun;
}

pub fn @"LIST"(ctx: *Ctx, xs: []u32) anyerror!u32 {
    var cur = wisp.nil;
    var i = xs.len;
    while (i > 0) : (i -= 1) {
        cur = try ctx.ctx.new(.duo, .{ .car = xs[i - 1], .cdr = cur });
    }
    return cur;
}

pub fn @"EQ"(ctx: *Ctx, x: u32, y: u32) anyerror!u32 {
    _ = ctx;
    return if (x == y) wisp.t else wisp.nil;
}

pub fn @"PRINT"(ctx: *Ctx, x: u32) anyerror!u32 {
    const out = std.io.getStdOut().writer();
    try dump.dump(ctx.ctx, out, x);
    try out.writeByte('\n');
    return x;
}

pub fn @"TYPE-OF"(ctx: *Ctx, x: u32) anyerror!u32 {
    const kwd = ctx.ctx.kwd;

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

pub fn @"ERROR"(ctx: *Ctx, xs: []u32) anyerror!u32 {
    const out = std.io.getStdOut().writer();
    try out.print("ERROR: ", .{});
    for (xs) |x| {
        try dump.dump(ctx.ctx, out, x);
        try out.writeByte(' ');
    }
    try out.writeByte('\n');
    return Err.Error;
}
