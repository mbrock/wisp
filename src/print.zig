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

const wisp = @import("./wisp.zig");
const Vat = wisp.Vat;

test "print one" {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    try list.writer().print("{}", .{1});
    try std.testing.expectEqualStrings("1", list.items);
}

pub fn expect(
    expected: []const u8,
    vat: *Vat,
    x: u32,
) !void {
    var actual = try printAlloc(std.testing.allocator, vat, x);
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

pub fn printAlloc(
    allocator: std.mem.Allocator,
    vat: *Vat,
    word: u32,
) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    try print(vat, list.writer(), word);
    return list.toOwnedSlice();
}

pub fn dump(prefix: []const u8, vat: *Vat, word: u32) !void {
    var s = try printAlloc(vat.gpa, vat, word);
    std.log.warn("{s} {s}", .{ prefix, s });
    vat.gpa.free(s);
}

pub fn print(
    vat: *Vat,
    out: anytype,
    x: u32,
) anyerror!void {
    switch (wisp.tagOf(x)) {
        .int => try out.print("{d}", .{x}),

        .sym => {
            const sym = try vat.row(.sym, x);
            const name = vat.strslice(sym.str);
            try out.print("{s}", .{name});
        },

        .str => {
            const s = vat.strslice(x);
            try out.print("\"{s}\"", .{s});
        },

        .duo => {
            try out.print("(", .{});
            var cur = x;

            loop: while (cur != wisp.nil) {
                var cons = try vat.row(.duo, cur);
                try print(vat, out, cons.car);
                switch (wisp.tagOf(cons.cdr)) {
                    .duo => {
                        try out.print(" ", .{});
                        cur = cons.cdr;
                    },
                    else => {
                        if (cons.cdr != wisp.nil) {
                            try out.print(" . ", .{});
                            try print(vat, out, cons.cdr);
                        }
                        break :loop;
                    },
                }
            }

            try out.print(")", .{});
        },

        .pkg => {
            try out.print("<package>", .{});
        },

        .chr, .sys, .fop, .mop => {},
        .fun, .vec, .ct0 => {},
    }
}

fn expectPrintResult(vat: *Vat, expected: []const u8, x: u32) !void {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    try print(vat, &writer, x);
    try std.testing.expectEqualStrings(expected, list.items);
}

test "print fixnum" {
    var vat = try Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();

    try expectPrintResult(&vat, "1", 1);
}

test "print lists" {
    var vat = try Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();

    try expectPrintResult(
        &vat,
        "(1 2 3)",
        try wisp.list(&vat, [_]u32{ 1, 2, 3 }),
    );

    try expectPrintResult(
        &vat,
        "(1 . 2)",
        try vat.new(.duo, .{ .car = 1, .cdr = 2 }),
    );
}

test "print symbols" {
    var vat = try Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();

    try expectPrintResult(
        &vat,
        "FOO",
        try vat.intern("FOO", vat.base()),
    );
}

// test "print structs" {
//     var vat = try Vat.init(std.testing.allocator);
//     defer vat.deinit();

//     try expectPrintResult(
//         &vat,
//         "«instance PACKAGE \"WISP\"»",
//         0,
//     );
// }

test "print strings" {
    var vat = try Vat.init(std.testing.allocator, .e1);
    defer vat.deinit();

    try expectPrintResult(
        &vat,
        "\"hello\"",
        try vat.newstr("hello"),
    );
}
