const std = @import("std");
const builtin = @import("builtin");

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
const eval = @import("./eval.zig");
const primops = @import("./primops.zig");

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

    var vat = try wisp.Vat.init(allocator, .e0);
    defer vat.deinit();

    try primops.load(&vat);

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
            _ = line;
            const term = try read(&vat, line);
            var ctx = eval.init(&vat, term);
            const result = try ctx.evaluate(1000);
            try print(&vat, stdout, result);
            try stdout.writeByte('\n');
        } else {
            try stdout.writeByte('\n');
            return;
        }
    }
}
