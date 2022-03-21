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

const Wisp = @import("./wisp.zig");
const Tidy = @import("./tidy.zig");
const Step = @import("./step.zig");
const Sexp = @import("./sexp.zig");
const Jets = @import("./jets.zig");
const Wasm = @import("./wasm.zig");
const Keys = @import("./keys.zig");

pub const crypto_always_getrandom: bool = true;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var orb = gpa.allocator();

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Keys);
}

pub fn main() anyerror!void {
    if (builtin.os.tag == .freestanding) {
        return;
    } else {
        return repl();
    }
}

fn readLine(allocator: std.mem.Allocator, stream: anytype) !?[]u8 {
    return stream.readUntilDelimiterOrEofAlloc(allocator, '\n', 4096);
}

fn readSexp(
    stream: anytype,
    allocator: std.mem.Allocator,
    heap: *Wisp.Heap,
) !?u32 {
    if (try readLine(allocator, stream)) |line| {
        return try Sexp.read(heap, line);
    } else {
        return null;
    }
}

pub fn repl() anyerror!void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var log = std.ArrayList(u8).init(orb);
    defer log.deinit();

    var heap = try Wisp.Heap.initWithLog(orb, .e0, &log);
    defer heap.deinit();

    try Jets.load(&heap);
    try heap.cook();

    // var baseTestRun = Step.initRun(try Sexp.read(&heap, "(base-test)"));

    // _ = try Step.evaluate(&heap, &baseTestRun, 1_000_000);

    repl: while (true) {
        try stdout.print("({d} log bytes)\n", .{log.items.len});
        log.clearAndFree();

        try stdout.writeAll("> ");

        var arena = std.heap.ArenaAllocator.init(orb);
        var tmp = arena.allocator();
        defer arena.deinit();

        if (try readSexp(stdin, tmp, &heap)) |term| {
            var run = Step.initRun(term);
            var step = Step{ .heap = &heap, .run = &run };
            _ = step;

            while (true) {
                if (Step.evaluate(&heap, &run, 0)) |val| {
                    const pretty = try Sexp.prettyPrint(&heap, val, 62);
                    defer heap.orb.free(pretty);
                    try stdout.print("{s}\n", .{pretty});
                    // try Tidy.gc(&heap);
                    continue :repl;
                } else |e| {
                    if (run.err == Wisp.nil) {
                        return e;
                    } else {
                        try Sexp.warn("Condition", &heap, run.err);
                        try Sexp.warn("Term", &heap, run.exp);
                        try Sexp.warn("Value", &heap, run.val);
                        try Sexp.warn("Environment", &heap, run.env);
                        try Sexp.warn("Context", &heap, run.way);

                        try stdout.writeAll("*> ");

                        return e;

                        // if (try readSexp(stdin, tmp, &heap)) |restart| {
                        //     run.err = Wisp.nil;
                        //     step.give(.exp, restart);
                        //     continue :term;
                        // } else {
                        //     return error.Nope;
                        // }
                    }
                }
            }
        } else {
            try stdout.writeByte('\n');
            break :repl;
        }
    }
}
