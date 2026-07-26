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

const File = @import("./file.zig");
const Jets = @import("./jets.zig");
const Sexp = @import("./sexp.zig");
const Wisp = @import("./wisp.zig");
const Tape = @import("./tape.zig");

const maxCodeSize = megabytes(1);

fn megabytes(bytes: usize) usize {
    return bytes * 1024 * 1024;
}

pub fn makeHeap(orb: Wisp.Orb) !Wisp.Heap {
    var heap = try Wisp.Heap.init(orb, .e0);

    try Jets.load(&heap);
    try heap.cookBase();
    try heap.cookRepl();

    return heap;
}

pub fn main(init: std.process.Init) anyerror!void {
    @import("./runtime.zig").setIo(init.io);
    const orb = init.gpa;

    var arena = std.heap.ArenaAllocator.init(orb);
    defer arena.deinit();

    const tmp = arena.allocator();

    var args = try init.minimal.args.iterateAllocator(tmp);

    var stdout_buf: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(init.io, &stdout_buf);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch {};

    var stderr_buf: [4096]u8 = undefined;
    var stderr_writer = std.Io.File.stderr().writer(init.io, &stderr_buf);
    const stderr = &stderr_writer.interface;
    defer stderr.flush() catch {};

    _ = args.skip(); // skip the program name

    const cmd = args.next() orelse return help(stderr);

    if (std.mem.eql(u8, cmd, "run")) {
        const path = args.next() orelse return help(stderr);
        const root = try File.cwd(tmp);
        const code = try root.readFileAlloc(init.io, path, tmp, .limited(maxCodeSize));

        var heap = try Wisp.Heap.fromEmbeddedCore(orb);
        defer heap.deinit();

        const result = try heap.load(code);
        const pretty = try Sexp.prettyPrint(&heap, result, 62);
        try stdout.print("{s}\n", .{pretty});
        try stdout.flush();
    } else if (std.mem.eql(u8, cmd, "keygen")) {
        const key = @import("./keys.zig").generate(init.io);
        try stdout.print("{s}\n", .{key.toZB32()});
        try stdout.flush();
    } else if (std.mem.eql(u8, cmd, "repl-zig")) {
        try @import("./repl.zig").repl();
    } else if (std.mem.eql(u8, cmd, "repl")) {
        var heap = try Wisp.Heap.fromEmbeddedCore(orb);
        _ = try heap.load("(repl)");
        try stderr.print(";; repl finished\n", .{});
        try stderr.flush();
        @breakpoint();
    } else if (std.mem.eql(u8, cmd, "eval")) {
        const code = args.next() orelse return help(stderr);

        var heap = try Wisp.Heap.fromEmbeddedCore(orb);
        const result = try heap.load(code);
        const pretty = try Sexp.prettyPrint(&heap, result, 62);
        try stdout.print("{s}\n", .{pretty});
        try stdout.flush();
    } else if (std.mem.eql(u8, cmd, "core")) {
        const name = args.next() orelse return help(stderr);

        var heap = try makeHeap(orb);
        try @import("./tidy.zig").gc(&heap, &.{});
        try @import("./tape.zig").save(&heap, name);
    } else if (std.mem.eql(u8, cmd, "load")) {
        const name = args.next() orelse return help(stderr);

        var heap = try @import("./tape.zig").load(orb, name);
        _ = try heap.load("(repl)");
    } else {
        try help(stderr);
    }
}

fn help(stderr: anytype) !void {
    try stderr.writeAll(
        \\usage: wisp <command>
        \\
        \\Commands:
        \\
        \\  wisp run        run a program
        \\  wisp repl       start a REPL
        \\  wisp core       save a boot core
        \\  wisp load       load a boot core
        \\  wisp keygen     print a unique key
        \\  wisp version    print the Wisp version
        \\
    );
}
