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

fn currentVersion() [32]u8 {
    var array: [32]u8 = @splat(0);
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

const pins_magic = "wispins1";
const pins_header_size = pins_magic.len + 2 * @sizeOf(u32);

pub fn save(heap: *Wisp.Heap, name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(heap.orb);
    defer arena.deinit();

    const cap = heap.cap;
    const rootdir = try @import("./file.zig").cwd(arena.allocator());
    const file = try rootdir.createFile(cap, name, .{});
    defer file.close(cap);

    var file_writer = file.writerStreaming(cap, &.{});
    try write(heap, &file_writer.interface);
    try file_writer.interface.flush();
}

fn makeHeader(heap: *Wisp.Heap) Header {
    var result = Header{
        .version = currentVersion(),
        .v08len = @as(u32, @intCast(heap.v08.items.len)),
        .v32len = @as(u32, @intCast(heap.v32.list.items.len)),
        .tabSizes = @splat(0),
        .era = @backingInt(heap.era),
        .pkg = heap.pkg,
        .commonStrings = heap.commonStrings,
    };

    inline for (Wisp.pointerTags, 0..) |tag, tagidx| {
        const tab = heap.tab(tag);
        result.tabSizes[tagidx] = @as(
            u32,
            @intCast(tab.list.len),
        );
    }

    return result;
}

pub fn byteSize(heap: *Wisp.Heap) usize {
    var size: usize = @sizeOf(Header);
    size += heap.v08.items.len;
    size += heap.v32.list.items.len * @sizeOf(u32);

    inline for (Wisp.pointerTags) |tag| {
        const tab = heap.tab(tag);
        size += tab.list.len *
            @sizeOf(Wisp.Row(tag));
    }

    size += pins_header_size;
    size += heap.pins.count() * 2 * @sizeOf(u32);
    return size;
}

pub fn write(
    heap: *Wisp.Heap,
    writer: *std.Io.Writer,
) !void {
    var tape_header = makeHeader(heap);
    try writer.writeAll(std.mem.asBytes(&tape_header));
    try writer.writeAll(heap.v08.items);
    try writer.writeAll(std.mem.sliceAsBytes(
        heap.v32.list.items,
    ));

    inline for (Wisp.pointerTags) |tag| {
        const tab = heap.tab(tag);
        inline for (
            comptime std.meta.fieldNames(Wisp.Row(tag)),
            0..,
        ) |_, j| {
            const col = tab.col(@as(
                Wisp.Col(tag),
                @fromBackingInt(@intCast(j)),
            ));
            try writer.writeAll(std.mem.sliceAsBytes(col));
        }
    }

    try writer.writeAll(pins_magic);
    try writer.writeInt(
        u32,
        @intCast(heap.pins.count()),
        .little,
    );
    try writer.writeInt(
        u32,
        @as(u32, heap.nextPinId),
        .little,
    );
    for (
        heap.pins.entries.items(.key),
        heap.pins.entries.items(.value),
    ) |id, value| {
        try writer.writeInt(u32, id, .little);
        try writer.writeInt(u32, value, .little);
    }
}

pub fn writeToMemory(
    heap: *Wisp.Heap,
    bytes: []u8,
) !usize {
    const size = byteSize(heap);
    if (bytes.len < size) return error.WriteFailed;

    var writer = std.Io.Writer.fixed(bytes[0..size]);
    try write(heap, &writer);
    return writer.end;
}

const Error = error{
    WIP,
    PackageMissing,
    InvalidPinsTrailer,
};

pub fn load(orb: Wisp.Orb, cap: std.Io, name: []const u8) !Wisp.Heap {
    var arena = std.heap.ArenaAllocator.init(orb);
    defer arena.deinit();

    const rootdir = try @import("./file.zig").cwd(arena.allocator());
    const bytes = try rootdir.readFileAlloc(
        cap,
        name,
        arena.allocator(),
        .unlimited,
    );
    return loadFromMemory(orb, cap, bytes);
}

pub fn loadFromMemory(orb: Wisp.Orb, cap: std.Io, bytes: []const u8) !Wisp.Heap {
    var reader = std.Io.Reader.fixed(bytes);
    const header_value = try reader.takeStruct(
        Header,
        .little,
    );

    var heap = Wisp.Heap{
        .orb = orb,
        .cap = cap,
        .era = @as(Wisp.Era, @fromBackingInt(@intCast(header_value.era))),
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
        try reader.readSliceAll(heap.v08.items);
    }

    if (header_value.v32len > 0) {
        try reader.readSliceAll(
            @as([*]u8, @ptrCast(heap.v32.list.items.ptr))[0 .. header_value.v32len * 4],
        );
    }

    inline for (Wisp.pointerTags, 0..) |tag, tagidx| {
        const tab = heap.tab(tag);
        const cnt = header_value.tabSizes[tagidx];
        try tab.list.ensureTotalCapacity(orb, cnt);
        tab.list.len = cnt;

        inline for (comptime std.meta.fieldNames(Wisp.Row(tag)), 0..) |_, j| {
            const col = tab.col(@as(Wisp.Col(tag), @fromBackingInt(@intCast(j))));
            if (col.len > 0) {
                try reader.readSliceAll(
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

    inline for (comptime std.meta.fieldNames(Wisp.Kwd)) |name| {
        const sym = try heap.intern(name, heap.base);
        @field(heap.kwd, name) = sym;
    }

    if (reader.bufferedLen() != 0) {
        const magic = try reader.takeArray(pins_magic.len);
        if (!std.mem.eql(u8, magic, pins_magic))
            return Error.InvalidPinsTrailer;

        const pin_count = std.mem.readInt(
            u32,
            try reader.takeArray(@sizeOf(u32)),
            .little,
        );
        const next_pin_id = std.mem.readInt(
            u32,
            try reader.takeArray(@sizeOf(u32)),
            .little,
        );
        heap.nextPinId = @intCast(next_pin_id);

        try heap.pins.ensureTotalCapacity(orb, pin_count);
        for (0..pin_count) |_| {
            const id = std.mem.readInt(
                u32,
                try reader.takeArray(@sizeOf(u32)),
                .little,
            );
            const value = std.mem.readInt(
                u32,
                try reader.takeArray(@sizeOf(u32)),
                .little,
            );
            try heap.pins.putNoClobber(
                orb,
                @intCast(id),
                value,
            );
        }
    }

    return heap;
}

test "in-memory tape preserves pinned values" {
    const testing = std.testing;

    var heap = try Wisp.Heap.init(
        testing.allocator,
        testing.io,
        .e0,
    );
    defer heap.deinit();

    const value = try heap.cons(1, 2);
    _ = try heap.newPin(value);

    const bytes = try testing.allocator.alloc(
        u8,
        byteSize(&heap),
    );
    defer testing.allocator.free(bytes);
    try testing.expectEqual(
        bytes.len,
        try writeToMemory(&heap, bytes),
    );

    var clone = try loadFromMemory(
        testing.allocator,
        testing.io,
        bytes,
    );
    defer clone.deinit();

    try testing.expectEqual(@as(u27, 2), clone.nextPinId);
    try testing.expectEqual(value, clone.pins.get(1).?);
}
