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

const wisp = @import("./ff-wisp.zig");
const Ctx = wisp.Ctx;

pub fn @"PROG1"(ctx: *Ctx, xs: []u32) anyerror!u32 {
    _ = ctx;
    return xs[0];
}

pub fn @"+"(ctx: *Ctx, xs: []u32) anyerror!u32 {
    _ = ctx;

    var result: i31 = 0;
    for (xs) |x| {
        result += @intCast(i31, x);
    }

    return @intCast(u32, result);
}

pub fn @"CONS"(ctx: *Ctx, car: u32, cdr: u32) anyerror!u32 {
    return ctx.new(.duo, .{ .car = car, .cdr = cdr });
}

pub fn @"CAR"(ctx: *Ctx, x: u32) anyerror!u32 {
    return ctx.get(.duo, .car, x);
}

pub fn @"CDR"(ctx: *Ctx, x: u32) anyerror!u32 {
    return ctx.get(.duo, .cdr, x);
}
