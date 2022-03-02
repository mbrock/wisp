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
const Eval = @import("./04-eval.zig");
const Rest = @import("./07-xops.zig").Rest;

pub fn QUOTE(this: *Eval, x: u32) anyerror!u32 {
    this.doneWithJob(x);
    return wisp.nah;
}

pub fn @"%MACRO-LAMBDA"(this: *Eval, par: u32, exp: u32) anyerror!u32 {
    this.job = .{
        .val = try this.ctx.new(.mac, .{
            .env = this.env,
            .par = par,
            .exp = exp,
        }),
    };

    return wisp.nah;
}

pub fn @"LET"(this: *Eval, bs: u32, e: u32) anyerror!u32 {
    if (bs == wisp.nil) {
        this.job = .{ .exp = e };
    } else {
        // find the first expression
        const b1 = try this.ctx.get(.duo, .car, bs);
        const e1 = try this.ctx.get(
            .duo,
            .car,
            try this.ctx.get(.duo, .cdr, b1),
        );

        this.job = .{ .exp = e1 };
        this.way = try this.ctx.new(.ct3, .{
            .hop = this.way,
            .env = this.env,
            .exp = e,
            .arg = bs,
            .dew = wisp.nil,
        });
    }

    return wisp.nah;
}

pub fn @"%LAMBDA"(this: *Eval, par: u32, exp: u32) anyerror!u32 {
    this.job = .{
        .val = try this.ctx.new(.fun, .{
            .env = this.env,
            .par = par,
            .exp = exp,
        }),
    };

    return wisp.nah;
}

pub fn IF(this: *Eval, exp: u32, yay: u32, nay: u32) anyerror!u32 {
    this.* = .{
        .ctx = this.ctx,
        .env = this.env,
        .job = .{ .exp = exp },
        .way = try this.ctx.new(.ct1, .{
            .hop = this.way,
            .env = this.env,
            .yay = yay,
            .nay = nay,
        }),
    };

    return wisp.nah;
}

pub fn PROGN(this: *Eval, rest: Rest) anyerror!u32 {
    if (rest.arg == wisp.nil) {
        this.doneWithJob(wisp.nil);
    } else {
        const duo = try this.ctx.row(.duo, rest.arg);
        this.job = .{ .exp = duo.car };
        this.way = try this.ctx.new(.ct2, .{
            .hop = this.way,
            .env = this.env,
            .exp = duo.cdr,
        });
    }

    return wisp.nah;
}
