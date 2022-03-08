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

const wisp = @import("./ff-wisp.zig");
const std = @import("std");

pub fn cwd(allocator: std.mem.Allocator) !std.fs.Dir {
    if (@import("builtin").os.tag == .wasi) {
        var preopens = std.fs.wasi.PreopenList.init(allocator);
        defer preopens.deinit();

        try preopens.populate();
        if (preopens.find(.{ .Dir = "." })) |x| {
            return std.fs.Dir{ .fd = x.fd };
        } else {
            return wisp.Oof.Err;
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
    if (x == wisp.nil)
        try out.print("nil", .{})
    else if (x == wisp.t)
        try out.print("t", .{})
    else if (x == wisp.nah)
        try out.print("nah", .{})
    else {
        const tag = wisp.tagOf(x);
        try out.print("{s}:", .{@tagName(tag)});
        switch (tag) {
            .int => {
                try out.print("{d}", .{x});
            },
            .sys, .chr, .jet => {
                try out.print("{d}", .{wisp.Imm.from(x).idx});
            },
            else => {
                const ptr = wisp.Ptr.from(x);
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

pub fn save(eval: *wisp.Eval, name: []const u8) !u32 {
    var room = std.heap.ArenaAllocator.init(eval.ctx.orb);

    defer room.deinit();

    var atom = try std.io.BufferedAtomicFile.create(
        room.allocator(),
        try cwd(room.allocator()),
        name,
        .{},
    );

    const file = atom.buffered_writer.writer();

    defer atom.destroy();

    const ctx = eval.ctx;

    try file.print("; -*- org -*-\n\n", .{});
    try file.print("* wisp\n", .{});
    try file.print("** ver 1\n\n", .{});
    try file.print("* eval\n", .{});
    try tellvar(file, "** era", @intCast(u32, @enumToInt(ctx.era)));
    try tellvar(file, "** bas", ctx.base);
    try tellvar(file, "** env", eval.bot.env);
    try tellvar(file, "** way", eval.bot.way);
    try tellvar(file, "** exp", eval.bot.exp);
    try tellvar(file, "** val", eval.bot.val);
    try tellvar(file, "** err", eval.bot.err);

    try file.print("\n", .{});
    try file.print("* v08 #{d}\n", .{ctx.v08.items.len});
    for (ctx.v08.items) |x| {
        try file.print("{d:0>2}", .{x});
    }
    try file.print("\n\n", .{});

    try file.print("* v32 #{d}\n", .{ctx.v32.list.items.len});
    for (ctx.v32.list.items) |x, i| {
        if (i > 0) try file.print(" ", .{});
        try tell(file, x);
    }
    try file.print("\n", .{});

    inline for (wisp.pointerTags) |tag| {
        const tab = ctx.tab(tag);
        if (tab.list.len > 0) {
            try file.print("\n", .{});
            try file.print("* {s} #{d}\n", .{
                @tagName(tag),
                tab.list.len,
            });

            inline for (std.meta.fields(wisp.Row(tag))) |_, j| {
                const col = @intToEnum(wisp.Col(tag), j);
                for (ctx.col(tag, col)) |x| {
                    try tell(file, x);
                    try file.print(" ", .{});
                }
                try file.print("\n", .{});
            }
        }
    }

    try atom.finish();

    return try eval.ctx.newv08(
        atom.atomic_file.dest_basename,
    );
}

test "save" {
    var ctx = try wisp.Eval.newTestCtx();
    defer ctx.deinit();

    try wisp.Eval.expectEval("\"foo.core\"", (
        \\ (save "foo.core")
    ));
}
