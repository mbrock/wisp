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
const xops = @import("./07-xops.zig");
const Ctx = wisp.Ctx;

test "print one" {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    try list.writer().print("{}", .{1});
    try std.testing.expectEqualStrings("1", list.items);
}

pub fn expect(
    expected: []const u8,
    ctx: *Ctx,
    x: u32,
) !void {
    var actual = try printAlloc(std.testing.allocator, ctx, x);
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

pub fn printAlloc(
    allocator: std.mem.Allocator,
    ctx: *Ctx,
    word: u32,
) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    try dump(ctx, list.writer(), word);
    return list.toOwnedSlice();
}

pub fn warn(prefix: []const u8, ctx: *Ctx, word: u32) !void {
    var s = try printAlloc(ctx.orb, ctx, word);
    std.log.warn("{s} {s}", .{ prefix, s });
    ctx.orb.free(s);
}

pub fn dump(ctx: *Ctx, out: anytype, x: u32) anyerror!void {
    return dump_(ctx, out, x);
}

pub fn dump_(
    ctx: *Ctx,
    out: anytype,
    x: u32,
) anyerror!void {
    switch (wisp.tagOf(x)) {
        .int => try out.print("{d}", .{x}),

        .sys => {
            switch (x) {
                wisp.nil => try out.print("NIL", .{}),
                wisp.t => try out.print("T", .{}),
                wisp.top => try out.print("%TOP", .{}),
                else => unreachable,
            }
        },

        .sym => {
            const sym = try ctx.row(.sym, x);
            const name = ctx.v08slice(sym.str);
            if (sym.pkg == wisp.nil) {
                try out.print("#:{s}", .{name});
            } else {
                try out.print("{s}", .{name});
            }
        },

        .v08 => {
            const s = ctx.v08slice(x);
            try out.print("\"{s}\"", .{s});
        },

        .v32 => {
            try out.print("#<", .{});
            const xs = try ctx.v32slice(x);
            for (xs) |y, i| {
                if (i > 0) try out.print(" ", .{});
                try dump(ctx, out, y);
            }
            try out.print(">", .{});
        },

        .duo => {
            try out.print("(", .{});
            var cur = x;

            loop: while (cur != wisp.nil) {
                var cons = try ctx.row(.duo, cur);
                try dump(ctx, out, cons.car);
                switch (wisp.tagOf(cons.cdr)) {
                    .duo => {
                        try out.print(" ", .{});
                        cur = cons.cdr;
                    },
                    else => {
                        if (cons.cdr != wisp.nil) {
                            try out.print(" . ", .{});
                            try dump(ctx, out, cons.cdr);
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

        .ct3 => {
            const ct3 = try ctx.row(.ct3, x);
            try out.print("<%if", .{});
            inline for (std.meta.fields(@TypeOf(ct3))) |field| {
                try out.print(" {s}=", .{field.name});
                try dump(ctx, out, @field(ct3, field.name));
            }
            try out.print(">", .{});
        },

        .ct0 => {
            const ct0 = try ctx.row(.ct0, x);
            try out.print("<%xs", .{});
            inline for (std.meta.fields(@TypeOf(ct0))) |field| {
                try out.print(" {s}=", .{field.name});
                try dump(ctx, out, @field(ct0, field.name));
            }
            try out.print(">", .{});
        },

        .ct2 => {
            const ct2 = try ctx.row(.ct2, x);
            try out.print("<%do", .{});
            inline for (std.meta.fields(@TypeOf(ct2))) |field| {
                try out.print(" {s}=", .{field.name});
                try dump(ctx, out, @field(ct2, field.name));
            }
            try out.print(">", .{});
        },

        .ct1 => {
            const ct1 = try ctx.row(.ct1, x);
            try out.print("<%if", .{});
            inline for (std.meta.fields(@TypeOf(ct1))) |field| {
                try out.print(" {s}=", .{field.name});
                try dump(ctx, out, @field(ct1, field.name));
            }
            try out.print(">", .{});
        },

        .fun => {
            const fun = try ctx.row(.fun, x);
            try out.print("<fun", .{});
            inline for (std.meta.fields(@TypeOf(fun))) |field| {
                try out.print(" {s}=", .{field.name});
                try dump(ctx, out, @field(fun, field.name));
            }
            try out.print(">", .{});
        },

        .jet => {
            const jet = xops.jets[wisp.Imm.from(x).idx];
            try out.print("<jet {s}>", .{jet.txt});
        },

        else => |t| try out.print("<{any}>", .{t}),
    }
}

fn expectPrintResult(ctx: *Ctx, expected: []const u8, x: u32) !void {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    try dump(ctx, &writer, x);
    try std.testing.expectEqualStrings(expected, list.items);
}

test "print fixnum" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    try expectPrintResult(&ctx, "1", 1);
}

test "print constants" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    try expectPrintResult(&ctx, "NIL", wisp.nil);
    try expectPrintResult(&ctx, "T", wisp.t);
}

test "print lists" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    try expectPrintResult(
        &ctx,
        "(1 2 3)",
        try wisp.list(&ctx, [_]u32{ 1, 2, 3 }),
    );

    try expectPrintResult(
        &ctx,
        "(1 . 2)",
        try ctx.new(.duo, .{ .car = 1, .cdr = 2 }),
    );
}

test "print symbols" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    try expectPrintResult(
        &ctx,
        "FOO",
        try ctx.intern("FOO", ctx.base),
    );
}

test "print uninterned symbols" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    try expectPrintResult(
        &ctx,
        "#:FOO",
        try ctx.newSymbol("FOO", wisp.nil),
    );
}

// test "print structs" {
//     var ctx = try Ctx.init(std.testing.allocator);
//     defer ctx.deinit();

//     try expectPrintResult(
//         &ctx,
//         "«instance PACKAGE \"WISP\"»",
//         0,
//     );
// }

test "print strings" {
    var ctx = try Ctx.init(std.testing.allocator, .e1);
    defer ctx.deinit();

    try expectPrintResult(
        &ctx,
        "\"hello\"",
        try ctx.newv08("hello"),
    );
}
