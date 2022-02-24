const std = @import("std");

const wisp = @import("./wisp.zig");
const Heap = wisp.Heap;

test "print one" {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    try list.writer().print("{}", .{1});
    try std.testing.expectEqualStrings("1", list.items);
}

pub fn expect(
    expected: []const u8,
    heap: *Heap,
    x: u32,
) !void {
    var actual = try printAlloc(std.testing.allocator, heap, x);
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

pub fn printAlloc(
    allocator: std.mem.Allocator,
    heap: *Heap,
    word: u32,
) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    try print(heap, list.writer(), word);
    return list.toOwnedSlice();
}

pub fn dump(prefix: []const u8, heap: *Heap, word: u32) !void {
    var s = try printAlloc(heap.gpa, heap, word);
    std.log.warn("{s} {s}", .{ prefix, s });
    heap.gpa.free(s);
}

pub fn print(
    heap: *Heap,
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
                    const offset = pointer.offset(.symbol, heap.semispace);
                    const nameIdx = heap.data.symbol.items(.name)[offset];
                    const name = heap.stringSlice(nameIdx);
                    try out.print("{s}", .{name});
                },

                .string => {
                    const s = heap.stringSlice(value);
                    try out.print("\"{s}\"", .{s});
                },

                .cons => {
                    try out.print("(", .{});
                    var cur = value;

                    loop: while (cur != wisp.NIL) {
                        var cons = try heap.deref(.cons, cur);
                        try print(heap, out, cons.car);
                        switch (wisp.Word.from(cons.cdr)) {
                            .pointer => |pointer2| {
                                switch (pointer2) {
                                    .cons => {
                                        try out.print(" ", .{});
                                        cur = cons.cdr;
                                    },
                                    else => {
                                        try out.print(" . ", .{});
                                        try print(heap, out, cons.cdr);
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
                                        try print(heap, out, cons.cdr);
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

                .argsPlan => {
                    try out.print("<argsplan>", .{});
                },
            }
        },
    }
}

fn expectPrintResult(heap: *Heap, expected: []const u8, x: u32) !void {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    try print(heap, &writer, x);
    try std.testing.expectEqualStrings(expected, list.items);
}

test "print fixnum" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    try expectPrintResult(&heap, "1", wisp.encodeFixnum(1));
}

test "print lists" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "(1 2 3)",
        try heap.list(
            [_]u32{
                wisp.encodeFixnum(1),
                wisp.encodeFixnum(2),
                wisp.encodeFixnum(3),
            },
        ),
    );

    try expectPrintResult(
        &heap,
        "(1 . 2)",
        try heap.append(.cons, .{
            .car = wisp.encodeFixnum(1),
            .cdr = wisp.encodeFixnum(2),
        }),
    );
}

test "print symbols" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "FOO",
        try heap.internStringInBasePackage("FOO"),
    );
}

// test "print structs" {
//     var heap = try Heap.init(std.testing.allocator);
//     defer heap.deinit();

//     try expectPrintResult(
//         &heap,
//         "«instance PACKAGE \"WISP\"»",
//         0,
//     );
// }

test "print strings" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "\"hello\"",
        try heap.addString("hello"),
    );
}
