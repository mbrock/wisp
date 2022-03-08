// -*- fill-column: 64; -*-
//
// This file is part of Wisp.
//
// Wisp is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version
// 3 of the License, or (at your option) any later version.
//
// Wisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General
// Public License along with Wisp. If not, see
// <https://www.gnu.org/licenses/>.
//

const std = @import("std");
const builtin = @import("builtin");

const wisp = @import("./ff-wisp.zig");
const read = @import("./05-read.zig").read;
const dump = @import("./06-dump.zig");
const Step = @import("./04-step.zig");
const xops = @import("./07-xops.zig");
const tidy = @import("./03-tidy.zig");

const wasm = @import("./e0-wasm.zig");

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

fn readLine(stream: anytype, orb: std.mem.Allocator) !?[]u8 {
    return stream.readUntilDelimiterOrEofAlloc(orb, '\n', 4096);
}

fn readSexp(
    stream: anytype,
    orb: std.mem.Allocator,
    heap: *wisp.Heap,
) !?u32 {
    if (try readLine(stream, orb)) |line| {
        return try read(heap, line);
    } else {
        return null;
    }
}

pub fn repl() anyerror!void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    var heap = try wisp.Heap.init(allocator, .e0);
    defer heap.deinit();

    try xops.load(&heap);
    try heap.cook();

    var baseTestRun = Step.initRun(try read(&heap, "(base-test)"));

    _ = try Step.evaluate(&heap, &baseTestRun, 1_000_000, false);

    repl: while (true) {
        try stdout.writeAll("> ");

        var arena = std.heap.ArenaAllocator.init(allocator);
        var tmp = arena.allocator();
        defer arena.deinit();

        if (try readSexp(stdin, tmp, &heap)) |term| {
            var run = Step.initRun(term);
            var step = Step{ .heap = &heap, .run = &run };

            term: while (true) {
                if (Step.evaluate(&heap, &run, 100_000, false)) |val| {
                    try dump.dump(&heap, stdout, val);
                    try stdout.writeByte('\n');
                    try tidy.gc(&heap);
                    continue :repl;
                } else |e| {
                    if (run.err == wisp.nil) {
                        return e;
                    } else {
                        try dump.warn("Condition", &heap, run.err);
                        try dump.warn("Term", &heap, run.exp);
                        try dump.warn("Value", &heap, run.val);
                        try dump.warn("Environment", &heap, run.env);

                        try stdout.writeAll("*> ");
                        if (try readSexp(stdin, tmp, &heap)) |restart| {
                            run.err = wisp.nil;
                            step.give(.exp, restart);
                            continue :term;
                        } else {
                            return error.Nope;
                        }
                    }
                }
            }
        } else {
            try stdout.writeByte('\n');
            break :repl;
        }
    }
}
