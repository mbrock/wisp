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

pub fn cwd(allocator: std.mem.Allocator) !std.fs.Dir {
    if (@import("builtin").os.tag == .wasi) {
        var preopens = std.fs.wasi.PreopenList.init(allocator);
        defer preopens.deinit();

        try preopens.populate();
        if (preopens.find(.{ .Dir = "." })) |x| {
            return std.fs.Dir{ .fd = x.fd };
        } else {
            return Wisp.Oof.Err;
        }
    } else {
        return std.fs.cwd();
    }
}

const colnum = blk: {
    var i = 0;

    inline for (Wisp.pointerTags) |tag| {
        i += std.meta.fields(Wisp.Row(tag)).len;
    }

    break :blk i;
};

fn currentVersion() [32]u8 {
    var array: [32]u8 = .{0} ** 32;
    std.mem.copy(u8, &array, "wisp tape v0.8.0\n");
    return array;
}

const Header = packed struct {
    version: [32]u8,
    v08len: u32,
    v32len: u32,
    tabSizes: [colnum]u32,
};

const empty: [1]u8 = .{0};

fn mkvec(ptr: anytype, len: usize) std.os.iovec_const {
    return if (len == 0) .{
        .iov_base = &empty,
        .iov_len = 0,
    } else .{
        .iov_base = @ptrCast([*]const u8, ptr),
        .iov_len = len,
    };
}

pub fn save(heap: *Wisp.Heap, name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(heap.orb);
    defer arena.deinit();

    var rootdir = try cwd(arena.allocator());
    var file = try rootdir.createFile(name, .{});

    var header = Header{
        .version = currentVersion(),
        .v08len = @intCast(u32, heap.v08.items.len),
        .v32len = @intCast(u32, heap.v32.list.items.len),
        .tabSizes = .{0} ** colnum,
    };

    var iovecs: [1 + 1 + 1 + colnum]std.os.iovec_const = undefined;

    iovecs[0] = mkvec(&header, @sizeOf(Header));
    iovecs[1] = mkvec(
        heap.v08.items.ptr,
        heap.v08.items.len,
    );

    iovecs[2] = mkvec(
        heap.v32.list.items.ptr,
        4 * heap.v32.list.items.len,
    );

    comptime var ii = 0;
    var i: u8 = 0;
    inline for (Wisp.pointerTags) |tag| {
        const tab = heap.tab(tag);
        inline for (std.meta.fields(Wisp.Row(tag))) |_, j| {
            const col = tab.col(@intToEnum(Wisp.Col(tag), j));
            if (col.len > 0) {
                header.tabSizes[ii] = @intCast(u32, col.len) * 4;
                iovecs[i + 3] = mkvec(col.ptr, col.len * 4);

                i += 1;
            }

            ii += 1;
        }
    }

    try file.writevAll(iovecs[0 .. i + 3]);
    file.close();
}
