const std = @import("std");

const base = @import("./base.zig");
const read = @import("./read.zig");
const print = @import("./print.zig");
const eval = @import("./eval.zig");

test {
    std.testing.refAllDecls(@This());
}

fn expectParsingRoundtrip(text: []const u8) !void {
    var ctx = try base.testWisp();
    defer ctx.heap.free();

    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    const x = try read.read(&ctx, text);

    try print.print(&ctx, &writer, x);
    try std.testing.expectEqualStrings(text, list.items);
}

test "read roundtrips" {
    try expectParsingRoundtrip("NIL");
    try expectParsingRoundtrip("123");
    try expectParsingRoundtrip("FOO");
    try expectParsingRoundtrip("\"Hello, world!\"");
}

test "read list roundtrips" {
    try expectParsingRoundtrip("(FOO)");
    try expectParsingRoundtrip("(FOO (1 2 3) BAR)");
    try expectParsingRoundtrip("(1 . 2)");
}

test "read code roundtrips" {
    try expectParsingRoundtrip("(DEFUN FOO (X Y) (+ X Y))");
}

pub fn main() anyerror!void {
    std.log.info("Wisp Zig", .{});
}
