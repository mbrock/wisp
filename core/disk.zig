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

const Wisp = @import("./wisp.zig");
const Step = @import("./step.zig");

pub fn cwd(allocator: std.mem.Allocator) !std.fs.Dir {
    if (@import("builtin").os.tag == .wasi) {
        var preopens = std.fs.wasi.PreopenList.init(allocator);
        defer preopens.deinit();

        try preopens.populate(null);
        if (preopens.find(.{ .Dir = "." })) |x| {
            return std.fs.Dir{ .fd = x.fd };
        } else {
            return Wisp.Oof.Err;
        }
    } else {
        return std.fs.cwd();
    }
}

pub fn readFileAlloc(
    allocator: std.mem.Allocator,
    path: []const u8,
) ![]u8 {
    const dir = try cwd(allocator);
    return dir.readFileAlloc(allocator, path, 1024 * 1024);
}

fn tell(out: anytype, x: u32) !void {
    if (x == Wisp.nil)
        try out.print("nil", .{})
    else if (x == Wisp.t)
        try out.print("t", .{})
    else if (x == Wisp.nah)
        try out.print("nah", .{})
    else {
        const tag = Wisp.tagOf(x);
        try out.print("{s}:", .{@tagName(tag)});
        switch (tag) {
            .int => {
                try out.print("{d}", .{x});
            },
            .sys, .chr, .jet => {
                try out.print("{d}", .{Wisp.Imm.from(x).idx});
            },
            else => {
                const ptr = Wisp.Ptr.from(x);
                try out.print("{d}", .{ptr.idx});
            },
        }
    }
}

fn tellvar(out: anytype, v: []const u8, x: u32) !void {
    try out.print("{s} ", .{v});
    try tell(out, x);
    try out.print("\n", .{});
}

pub fn save(step: *Step, name: []const u8) !u32 {
    var room = std.heap.ArenaAllocator.init(step.heap.orb);

    defer room.deinit();

    var atom = try std.io.BufferedAtomicFile.create(
        room.allocator(),
        try cwd(room.allocator()),
        name,
        .{},
    );

    const file = atom.buffered_writer.writer();

    defer atom.destroy();

    const heap = step.heap;

    try file.print("; -*- org -*-\n\n", .{});
    try file.print("* wisp\n", .{});
    try file.print("** ver 1\n\n", .{});
    try file.print("* step\n", .{});
    try tellvar(file, "** era", @intCast(@intFromEnum(heap.era)));
    try tellvar(file, "** bas", heap.base);
    try tellvar(file, "** env", step.run.env);
    try tellvar(file, "** way", step.run.way);
    try tellvar(file, "** exp", step.run.exp);
    try tellvar(file, "** val", step.run.val);
    try tellvar(file, "** err", step.run.err);

    try file.print("\n", .{});
    try file.print("* v08 #{d}\n", .{heap.v08.items.len});
    for (heap.v08.items) |x| {
        try file.print("{d:0>2}", .{x});
    }
    try file.print("\n\n", .{});

    try file.print("* v32 #{d}\n", .{heap.v32.list.items.len});
    for (heap.v32.list.items, 0..) |x, i| {
        if (i > 0) try file.print(" ", .{});
        try tell(file, x);
    }
    try file.print("\n", .{});

    inline for (Wisp.pointerTags) |tag| {
        const tab = heap.tab(tag);
        if (tab.list.len > 0) {
            try file.print("\n", .{});
            try file.print("* {s} #{d}\n", .{
                @tagName(tag),
                tab.list.len,
            });

            inline for (std.meta.fields(Wisp.Row(tag)), 0..) |_, j| {
                const col = @as(Wisp.Col(tag), @enumFromInt(j));
                for (heap.col(tag, col)) |x| {
                    try tell(file, x);
                    try file.print(" ", .{});
                }
                try file.print("\n", .{});
            }
        }
    }

    try atom.finish();

    return try step.heap.newv08(
        atom.atomic_file.dest_basename,
    );
}

// test "save" {
//     var heap = try Step.newTestHeap();
//     defer heap.deinit();

//     try Step.expectEval("ok", (
//         \\ (returning 'ok (save))
//     ));
// }
