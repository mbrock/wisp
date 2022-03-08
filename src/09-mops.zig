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

const wisp = @import("./ff-wisp.zig");
const Eval = @import("./04-eval.zig");
const Rest = @import("./07-xops.zig").Rest;

pub fn QUOTE(job: *Eval, x: u32) anyerror!void {
    job.give(.val, x);
}

pub fn FUNCTION(job: *Eval, x: u32) anyerror!void {
    const fun = try job.heap.get(.sym, .fun, x);
    job.give(.val, fun);
}

pub fn @"%MACRO-LAMBDA"(job: *Eval, par: u32, exp: u32) anyerror!void {
    job.give(.val, try job.heap.new(.mac, .{
        .env = job.bot.env,
        .par = par,
        .exp = exp,
    }));
}

pub fn @"LET"(job: *Eval, bs: u32, e: u32) anyerror!void {
    if (bs == wisp.nil) {
        job.give(.exp, e);
    } else {
        // Find the first symbol and expression.
        const duo = try job.heap.row(.duo, bs);
        const let = duo.car;
        const letduo = try job.heap.row(.duo, let);
        const letsym = letduo.car;
        const letexp = try job.heap.get(.duo, .car, letduo.cdr);

        const acc = try job.heap.new(.duo, .{
            .car = letsym,
            .cdr = try job.heap.new(.duo, .{
                .car = e,
                .cdr = wisp.nil,
            }),
        });

        job.bot.way = try job.heap.new(.ktx, .{
            .hop = job.bot.way,
            .env = job.bot.env,
            .fun = job.heap.kwd.LET,
            .acc = acc,
            .arg = duo.cdr,
        });

        job.give(.exp, letexp);
    }
}

pub fn @"LAMBDA"(job: *Eval, par: u32, exp: u32) anyerror!void {
    job.give(.val, try job.heap.new(.fun, .{
        .env = job.bot.env,
        .par = par,
        .exp = exp,
    }));
}

pub fn IF(job: *Eval, exp: u32, yay: u32, nay: u32) anyerror!void {
    const ktx = try job.heap.new(.ktx, .{
        .hop = job.bot.way,
        .env = job.bot.env,
        .fun = job.heap.kwd.IF,
        .acc = wisp.nil,
        .arg = try job.heap.new(.duo, .{
            .car = yay,
            .cdr = nay,
        }),
    });

    job.bot.way = ktx;
    job.give(.exp, exp);
}

pub fn PROGN(job: *Eval, rest: Rest) anyerror!void {
    if (rest.arg == wisp.nil) {
        job.give(.val, wisp.nil);
    } else {
        const duo = try job.heap.row(.duo, rest.arg);
        const ktx = try job.heap.new(.ktx, .{
            .hop = job.bot.way,
            .env = job.bot.env,
            .fun = job.heap.kwd.PROGN,
            .acc = wisp.nil,
            .arg = duo.cdr,
        });

        job.bot.way = ktx;
        job.give(.exp, duo.car);
    }
}

pub fn DEFPACKAGE(job: *Eval, name: u32, conf: u32) anyerror!void {
    const txt = try job.heap.get(.sym, .str, name);
    job.give(.val, try job.heap.defpackage(txt, conf));
}

pub fn @"IN-PACKAGE"(job: *Eval, pkgsym: u32) anyerror!void {
    const v08 = try job.heap.get(.sym, .str, pkgsym);
    const str = try job.heap.v08slice(v08);

    if (job.heap.pkgmap.get(str)) |pkg| {
        job.heap.pkg = pkg;
        job.give(.val, pkg);
    } else {
        try job.fail(&[_]u32{ job.heap.kwd.@"PACKAGE-ERROR", pkgsym });
    }
}

// pub fn QUASIQUOTE(job: *Eval, exp: u32) anyerror!void {
//     // `(x ,y) => (list (quote x) y)

// }
