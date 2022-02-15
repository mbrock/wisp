const std = @import("std");

const base = @import("./base.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
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

    const x = try read(&ctx, text);

    try print(&ctx, &writer, x);
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
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var heap = try base.emptyHeap(std.heap.page_allocator, 1024 * 1024 * 16);
    var ctx = try base.start(heap);

    while (true) {
        try stdout.writeAll("wisp> ");

        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        const lineOrEof = try stdin.readUntilDelimiterOrEofAlloc(
            allocator,
            '\n',
            4096,
        );

        if (lineOrEof) |line| {
            const term = try read(&ctx, line);
            const result = try eval.evalTopLevel(&ctx, term, 100);
            try print(&ctx, stdout, result);
            try stdout.writeByte('\n');
        } else {
            try stdout.writeByte('\n');
            return;
        }
    }
}
