const std = @import("std");

const wisp = @import("./wisp.zig");

test "print one" {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    try list.writer().print("{}", .{1});
    try std.testing.expectEqualStrings("1", list.items);
}

pub fn expect(
    expected: []const u8,
    data: *wisp.Data,
    x: u32,
) !void {
    var actual = try printAlloc(std.testing.allocator, data, x);
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

pub fn printAlloc(
    allocator: std.mem.Allocator,
    data: *wisp.Data,
    word: u32,
) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    try print(data, list.writer(), word);
    return list.toOwnedSlice();
}

pub fn dump(prefix: []const u8, ctx: *wisp.Data, word: u32) !void {
    var s = try printAlloc(ctx.gpa, ctx, word);
    std.log.warn("{s} {s}", .{ prefix, s });
    ctx.gpa.free(s);
}

pub fn print(
    ctx: *wisp.Data,
    out: anytype,
    value: u32,
) anyerror!void {
    switch (wisp.Word.from(value)) {
        .immediate => |immediate| {
            switch (immediate) {
                .nil => {
                    try out.print("NIL", .{});
                },

                .fixnum => |x| {
                    try out.print("{d}", .{x});
                },

                .primop => {
                    try out.print("<primop>", .{});
                },

                .glyph => {
                    try out.print("<glyph>", .{});
                },

                else => unreachable,
            }
        },

        .pointer => |pointer| {
            switch (pointer) {
                .symbol => {
                    const offset = pointer.offset(.symbol, ctx.semispace);
                    const nameIdx = ctx.stuff.symbol.items(.name)[offset];
                    const name = ctx.stringSlice(nameIdx);
                    try out.print("{s}", .{name});
                },

                .string => {
                    const s = ctx.stringSlice(value);
                    try out.print("\"{s}\"", .{s});
                },

                .cons => {
                    try out.print("(", .{});
                    var cur = value;

                    loop: while (cur != wisp.NIL) {
                        var cons = try ctx.deref(.cons, cur);
                        try print(ctx, out, cons.car);
                        switch (wisp.Word.from(cons.cdr)) {
                            .pointer => |pointer2| {
                                switch (pointer2) {
                                    .cons => {
                                        try out.print(" ", .{});
                                        cur = cons.cdr;
                                    },
                                    else => {
                                        try out.print(" . ", .{});
                                        try print(ctx, out, cons.cdr);
                                        break :loop;
                                    },
                                }
                            },

                            .immediate => |immediate| {
                                switch (immediate) {
                                    .nil => {
                                        break :loop;
                                    },
                                    else => {
                                        try out.print(" . ", .{});
                                        try print(ctx, out, cons.cdr);
                                        break :loop;
                                    },
                                }
                            },
                        }
                    }

                    try out.print(")", .{});
                },

                .package => {
                    try out.print("<package>", .{});
                },
            }
        },
    }
}

fn expectPrintResult(ctx: *wisp.Data, expected: []const u8, x: u32) !void {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    try print(ctx, &writer, x);
    try std.testing.expectEqualStrings(expected, list.items);
}

test "print fixnum" {
    var ctx = try wisp.Data.init(std.testing.allocator);
    defer ctx.deinit();

    try expectPrintResult(&ctx, "1", wisp.encodeFixnum(1));
}

test "print lists" {
    var ctx = try wisp.Data.init(std.testing.allocator);
    defer ctx.deinit();

    try expectPrintResult(
        &ctx,
        "(1 2 3)",
        try ctx.list(
            [_]u32{
                wisp.encodeFixnum(1),
                wisp.encodeFixnum(2),
                wisp.encodeFixnum(3),
            },
        ),
    );

    try expectPrintResult(
        &ctx,
        "(1 . 2)",
        try ctx.append(.cons, .{
            .car = wisp.encodeFixnum(1),
            .cdr = wisp.encodeFixnum(2),
        }),
    );
}

test "print symbols" {
    var ctx = try wisp.Data.init(std.testing.allocator);
    defer ctx.deinit();

    try expectPrintResult(
        &ctx,
        "FOO",
        try ctx.internStringInBasePackage("FOO"),
    );
}

// test "print structs" {
//     var ctx = try wisp.Data.init(std.testing.allocator);
//     defer ctx.deinit();

//     try expectPrintResult(
//         &ctx,
//         "«instance PACKAGE \"WISP\"»",
//         0,
//     );
// }

test "print strings" {
    var ctx = try wisp.Data.init(std.testing.allocator);
    defer ctx.deinit();

    try expectPrintResult(
        &ctx,
        "\"hello\"",
        try ctx.addString("hello"),
    );
}
