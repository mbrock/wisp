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

pub fn @"FOO"(ctx: *Ctx, x: u32, y: u32) anyerror!u32 {
    _ = ctx;
    return try ctx.new(.duo, .{
        .car = x,
        .cdr = try ctx.new(.duo, .{
            .car = y,
            .cdr = try ctx.new(.duo, .{
                .car = 1,
                .cdr = wisp.nil,
            }),
        }),
    });
}
