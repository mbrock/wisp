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

const Wisp = @import("./wisp.zig");
const Step = @import("./step.zig");
const Jets = @import("./jets.zig");

const Rest = Jets.Rest;

pub fn QUOTE(step: *Step, x: u32) anyerror!void {
    step.give(.val, x);
}

pub fn FUNCTION(step: *Step, x: u32) anyerror!void {
    const fun = try step.heap.get(.sym, .fun, x);
    step.give(.val, fun);
}

pub fn @"%MACRO-FN"(step: *Step, par: u32, exp: u32) anyerror!void {
    step.give(.val, try step.heap.new(.mac, .{
        .env = step.run.env,
        .par = par,
        .exp = exp,
        .sym = Wisp.nil,
    }));
}

pub fn @"LET"(step: *Step, bs: u32, e: u32) anyerror!void {
    if (bs == Wisp.nil) {
        step.give(.exp, e);
    } else {
        // Find the first symbol and expression.
        const duo = try step.heap.row(.duo, bs);
        const let = duo.car;
        const letduo = try step.heap.row(.duo, let);
        const letsym = letduo.car;
        const letexp = try step.heap.get(.duo, .car, letduo.cdr);

        const acc = try step.heap.cons(
            letsym,
            try step.heap.cons(e, Wisp.nil),
        );

        step.run.way = try step.heap.new(.ktx, .{
            .hop = step.run.way,
            .env = step.run.env,
            .fun = step.heap.kwd.LET,
            .acc = acc,
            .arg = duo.cdr,
        });

        step.give(.exp, letexp);
    }
}

pub fn @"FN"(step: *Step, par: u32, exp: u32) anyerror!void {
    step.give(.val, try step.heap.new(.fun, .{
        .env = step.run.env,
        .par = par,
        .exp = exp,
        .sym = Wisp.nil,
    }));
}

pub fn IF(step: *Step, exp: u32, yay: u32, nay: u32) anyerror!void {
    const ktx = try step.heap.new(.ktx, .{
        .hop = step.run.way,
        .env = step.run.env,
        .fun = step.heap.kwd.IF,
        .acc = Wisp.nil,
        .arg = try step.heap.cons(yay, nay),
    });

    step.run.way = ktx;
    step.give(.exp, exp);
}

pub fn PROGN(step: *Step, rest: Rest) anyerror!void {
    if (rest.arg == Wisp.nil) {
        step.give(.val, Wisp.nil);
    } else {
        const duo = try step.heap.row(.duo, rest.arg);
        const ktx = try step.heap.new(.ktx, .{
            .hop = step.run.way,
            .env = step.run.env,
            .fun = step.heap.kwd.PROGN,
            .acc = Wisp.nil,
            .arg = duo.cdr,
        });

        step.run.way = ktx;
        step.give(.exp, duo.car);
    }
}

pub fn DEFPACKAGE(step: *Step, name: u32, conf: u32) anyerror!void {
    const txt = try step.heap.get(.sym, .str, name);
    step.give(.val, try step.heap.defpackage(txt, conf));
}

pub fn @"IN-PACKAGE"(step: *Step, pkgsym: u32) anyerror!void {
    const v08 = try step.heap.get(.sym, .str, pkgsym);
    const str = try step.heap.v08slice(v08);

    if (step.heap.pkgmap.get(str)) |pkg| {
        step.heap.pkg = pkg;
        step.give(.val, pkg);
    } else {
        try step.fail(&[_]u32{ step.heap.kwd.@"PACKAGE-ERROR", pkgsym });
    }
}
