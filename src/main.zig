const std = @import("std");
const builtin = @import("builtin");

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;

test {
    std.testing.refAllDecls(@This());
}

pub fn main() anyerror!void {
    if (builtin.os.tag == .freestanding) {
        return;
    } else {
        return repl();
    }
}

pub fn repl() anyerror!void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    var ctx = try wisp.Data.init(allocator);
    defer ctx.deinit();

    while (true) {
        try stdout.writeAll("wisp> ");

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        const lineOrEof = try stdin.readUntilDelimiterOrEofAlloc(
            arena.allocator(),
            '\n',
            4096,
        );

        if (lineOrEof) |line| {
            const term = try read(&ctx, line);
            try print(&ctx, stdout, term);
            try stdout.writeByte('\n');
        } else {
            try stdout.writeByte('\n');
            return;
        }
    }
}
