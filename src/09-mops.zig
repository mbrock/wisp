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

pub fn QUOTE(job: *Eval, x: u32) anyerror!void {
    job.give(.val, x);
}

pub fn @"%MACRO-LAMBDA"(job: *Eval, par: u32, exp: u32) anyerror!void {
    job.give(.val, try job.ctx.new(.mac, .{
        .env = job.env,
        .par = par,
        .exp = exp,
    }));
}

pub fn @"LET"(job: *Eval, bs: u32, e: u32) anyerror!void {
    if (bs == wisp.nil) {
        job.give(.exp, e);
    } else {
        // find the first expression
        const b1 = try job.ctx.get(.duo, .car, bs);
        const e1 = try job.ctx.get(
            .duo,
            .car,
            try job.ctx.get(.duo, .cdr, b1),
        );

        job.way = try job.ctx.new(.ct3, .{
            .hop = job.way,
            .env = job.env,
            .exp = e,
            .arg = bs,
            .dew = wisp.nil,
        });

        job.give(.exp, e1);
    }
}

pub fn @"%LAMBDA"(job: *Eval, par: u32, exp: u32) anyerror!void {
    job.give(.val, try job.ctx.new(.fun, .{
        .env = job.env,
        .par = par,
        .exp = exp,
    }));
}

pub fn IF(job: *Eval, exp: u32, yay: u32, nay: u32) anyerror!void {
    job.give(.exp, exp);
    job.way = try job.ctx.new(.ct1, .{
        .hop = job.way,
        .env = job.env,
        .yay = yay,
        .nay = nay,
    });
}

pub fn PROGN(job: *Eval, rest: Rest) anyerror!void {
    if (rest.arg == wisp.nil) {
        job.give(.val, wisp.nil);
    } else {
        const duo = try job.ctx.row(.duo, rest.arg);

        job.way = try job.ctx.new(.ct2, .{
            .hop = job.way,
            .env = job.env,
            .exp = duo.cdr,
        });

        job.give(.exp, duo.car);
    }
}
