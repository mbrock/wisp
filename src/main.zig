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
const builtin = @import("builtin");

const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const print = @import("./print.zig").print;
const eval = @import("./eval.zig");
const ops = @import("./ops.zig");
const tidy = @import("./tidy.zig");

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

    var ctx = try wisp.Ctx.init(allocator, .e0);
    defer ctx.deinit();

    try ops.load(&ctx);

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
            const exp = try read(&ctx, line);
            var exe = eval.init(&ctx, exp);
            const val = try exe.evaluate(1000);
            try print(&ctx, stdout, val);
            try stdout.writeByte('\n');
            try tidy.tidy(&ctx);
        } else {
            try stdout.writeByte('\n');
            return;
        }
    }
}
