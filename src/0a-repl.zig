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

const wisp = @import("./ff-wisp.zig");
const read = @import("./05-read.zig").read;
const dump = @import("./06-dump.zig");
const eval = @import("./04-eval.zig");
const xops = @import("./07-xops.zig");
const tidy = @import("./03-tidy.zig");

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

    try xops.load(&ctx);
    try ctx.cook();

    _ = try eval.init(
        &ctx,
        try read(&ctx, "(base-test)"),
    ).evaluate(1_000_000, false);

    while (true) {
        try stdout.writeAll("> ");

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

            loop: while (true) {
                if (exe.evaluate(1000, false)) |val| {
                    try dump.dump(&ctx, stdout, val);
                    try stdout.writeByte('\n');
                    try tidy.tidy(&ctx);
                    break :loop;
                } else |e| {
                    if (exe.err == wisp.nil) {
                        exe.err = try ctx.newv32(&[_]u32{
                            ctx.kwd.BUG,
                            try ctx.intern(@errorName(e), ctx.base),
                        });
                    }

                    try dump.warn("Condition", &ctx, exe.err);
                    try dump.warn(
                        "Failed term",
                        &ctx,
                        exe.job.exp,
                    );

                    try stdout.writeAll("*> ");
                    if (try stdin.readUntilDelimiterOrEofAlloc(
                        arena.allocator(),
                        '\n',
                        4096,
                    )) |l2| {
                        exe.err = wisp.nil;
                        const rexp = try read(&ctx, l2);
                        exe.job = .{ .exp = rexp };
                        continue :loop;
                    } else {
                        return error.Nope;
                    }
                }
            }
        } else {
            try stdout.writeByte('\n');
            return;
        }
    }
}
