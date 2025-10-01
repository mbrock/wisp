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

const rownum = Wisp.pointerTags.len;

const colnum = blk: {
    var i = 0;

    for (Wisp.pointerTags) |tag| {
        i += std.meta.fields(Wisp.Row(tag)).len;
    }

    break :blk i;
};

fn currentVersion() [32]u8 {
    var array: [32]u8 = .{0} ** 32;
    std.mem.copyForwards(u8, &array, "wisp tape v0.8.0\n");
    return array;
}

const Header = extern struct {
    version: [32]u8,
    era: u32,
    pkg: u32,
    v08len: u32,
    v32len: u32,
    commonStrings: Wisp.CommonStrings,
    tabSizes: [rownum]u32,
};

var empty: [1]u8 = .{0};

fn mkvec_const(ptr: anytype, len: usize) std.posix.iovec_const {
    return if (len == 0) .{
        .base = &empty,
        .len = 0,
    } else .{
        .base = @as([*]const u8, @ptrCast(ptr)),
        .len = len,
    };
}

fn mkvec(ptr: anytype, len: usize) std.posix.iovec {
    return if (len == 0) .{
        .base = &empty,
        .len = 0,
    } else .{
        .base = @as([*]u8, @ptrCast(ptr)),
        .len = len,
    };
}

pub fn save(heap: *Wisp.Heap, name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(heap.orb);
    defer arena.deinit();

    var rootdir = try @import("./file.zig").cwd(arena.allocator());
    var file = try rootdir.createFile(name, .{});
    defer file.close();

    var header = Header{
        .version = currentVersion(),
        .v08len = @as(u32, @intCast(heap.v08.items.len)),
        .v32len = @as(u32, @intCast(heap.v32.list.items.len)),
        .tabSizes = .{0} ** rownum,
        .era = @intFromEnum(heap.era),
        .pkg = heap.pkg,
        .commonStrings = heap.commonStrings,
    };

    var iovecs: [1 + 1 + 1 + colnum]std.posix.iovec_const = undefined;

    iovecs[0] = mkvec_const(&header, @sizeOf(Header));
    iovecs[1] = mkvec_const(
        heap.v08.items.ptr,
        heap.v08.items.len,
    );

    iovecs[2] = mkvec_const(
        heap.v32.list.items.ptr,
        4 * heap.v32.list.items.len,
    );

    var i: u8 = 0;
    inline for (Wisp.pointerTags, 0..) |tag, tagidx| {
        const tab = heap.tab(tag);
        inline for (std.meta.fields(Wisp.Row(tag)), 0..) |_, j| {
            const col = tab.col(@as(Wisp.Col(tag), @enumFromInt(j)));
            if (col.len > 0) {
                header.tabSizes[tagidx] = @as(u32, @intCast(col.len));
                iovecs[i + 3] = mkvec_const(col.ptr, col.len * 4);

                i += 1;
            }
        }
    }

    try file.writevAll(iovecs[0 .. i + 3]);
}

const Error = error{
    WIP,
    PackageMissing,
};

pub fn load(orb: Wisp.Orb, name: []const u8) !Wisp.Heap {
    var arena = std.heap.ArenaAllocator.init(orb);
    defer arena.deinit();

    var rootdir = try @import("./file.zig").cwd(arena.allocator());
    var file = try rootdir.openFile(name, .{});
    defer file.close();
    const bytes = try file.readToEndAlloc(arena.allocator(), std.math.maxInt(usize));
    return loadFromMemory(orb, bytes);
}

pub fn loadFromMemory(orb: Wisp.Orb, bytes: []const u8) !Wisp.Heap {
    var stream = std.io.fixedBufferStream(bytes);
    var reader = stream.reader();
    const header = try reader.readStruct(Header);
    const header_value = header;

    var heap = Wisp.Heap{
        .orb = orb,
        .era = @as(Wisp.Era, @enumFromInt(header_value.era)),
        .pkg = header_value.pkg,
        .commonStrings = header_value.commonStrings,
        .base = 0,
        .keywordPackage = 0,
        .keyPackage = 0,
    };

    try heap.v08.ensureTotalCapacity(orb, header_value.v08len);
    heap.v08.items.len = header_value.v08len;

    try heap.v32.list.ensureTotalCapacity(orb, header_value.v32len);
    heap.v32.list.items.len = header_value.v32len;

    if (header_value.v08len > 0) {
        try reader.readNoEof(heap.v08.items);
    }

    if (header_value.v32len > 0) {
        try reader.readNoEof(
            @as([*]u8, @ptrCast(heap.v32.list.items.ptr))[0 .. header_value.v32len * 4],
        );
    }

    inline for (Wisp.pointerTags, 0..) |tag, tagidx| {
        const tab = heap.tab(tag);
        const cnt = header_value.tabSizes[tagidx];
        try tab.list.ensureTotalCapacity(orb, cnt);
        tab.list.len = cnt;

        inline for (std.meta.fields(Wisp.Row(tag)), 0..) |_, j| {
            const col = tab.col(@as(Wisp.Col(tag), @enumFromInt(j)));
            if (col.len > 0) {
                try reader.readNoEof(
                    @as([*]u8, @ptrCast(col.ptr))[0 .. col.len * 4],
                );
            }
        }
    }

    {
        // find packages and put them in the package map
        for (heap.tab(.pkg).col(.nam), 0..) |pkgname, i| {
            const str = try orb.dupe(u8, try heap.v08slice(pkgname));
            try heap.pkgmap.putNoClobber(
                orb,
                str,
                Wisp.Ptr.make(.pkg, @as(u26, @intCast(i)), heap.era).word(),
            );
        }

        heap.base = heap.pkgmap.get("WISP") orelse return Error.PackageMissing;
        heap.keywordPackage = heap.pkgmap.get("KEYWORD") orelse return Error.PackageMissing;
        heap.keyPackage = heap.pkgmap.get("KEY") orelse return Error.PackageMissing;
    }

    inline for (std.meta.fields(Wisp.Kwd)) |s| {
        const sym = try heap.intern(s.name, heap.base);
        @field(heap.kwd, s.name) = sym;
    }

    return heap;
}
