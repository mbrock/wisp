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

const GC = @This();

old: *wisp.Vat,
new: wisp.Vat,

const std = @import("std");
const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const printer = @import("./print.zig");

pub fn tidy(vat: *wisp.Vat) !void {
    const n1 = vat.bytesize();
    var gc = try init(vat);
    defer gc.deinit();
    try gc.copyRoots();
    try gc.scavenge();
    vat.* = gc.finalize();
    const n2 = vat.bytesize();

    std.log.info("gc: before {d}, after {d}", .{ n1, n2 });
}

pub fn init(old: *wisp.Vat) !GC {
    const era = old.era.flip();
    return GC{
        .old = old,
        .new = wisp.Vat{
            .era = era,
            .orb = old.orb,
            .bin = old.bin,
            .tabs = wisp.Tabs.init(era),
            .base = 0xdeadbeef,
        },
    };
}

pub fn deinit(gc: *GC) void {
    _ = gc;
}

fn finalize(gc: *GC) wisp.Vat {
    gc.old.bin = .{};
    gc.old.deinit();
    return gc.new;
}

fn copyRoots(gc: *GC) !void {
    gc.new.base = try gc.copy(gc.old.base);
}

fn copy(gc: *GC, x: u32) !u32 {
    return switch (wisp.tagOf(x)) {
        .int, .chr, .sys, .fop, .mop => x,
        .sym => gc.copyRow(.sym, x),
        .duo => gc.copyRow(.duo, x),
        .fun => gc.copyRow(.fun, x),
        .vec => gc.copyRow(.vec, x),
        .str => gc.copyRow(.str, x),
        .pkg => gc.copyRow(.pkg, x),
        .ct0 => gc.copyRow(.ct0, x),
    };
}

fn nthField(comptime tag: wisp.Tag, i: comptime_int) []const u8 {
    return @tagName(@intToEnum(std.meta.FieldEnum(wisp.Row(tag)), i));
}

fn copyRow(gc: *GC, comptime tag: wisp.Tag, x: u32) !u32 {
    const ptr = wisp.Ptr.from(x);
    if (ptr.era == gc.new.era) return x;

    var row = try gc.old.row(tag, x);

    var c0 = gc.old.col(tag, @intToEnum(wisp.TagCol(tag), 0));
    var c1 = gc.old.col(tag, @intToEnum(wisp.TagCol(tag), 1));
    if (c0[ptr.idx] == wisp.zap) return c1[ptr.idx];

    const tab = gc.new.tab(tag);
    const new = try tab.new(gc.new.orb, row);

    c0[ptr.idx] = wisp.zap;
    c1[ptr.idx] = new;

    return new;
}

fn scavenge(gc: *GC) !void {
    while (!gc.isDone()) {
        inline for (wisp.pointerTags) |tag| {
            try gc.scavengeTag(tag);
        }
    }
}

fn isDone(gc: *GC) bool {
    inline for (wisp.pointerTags) |tag| {
        if (gc.new.tab(tag).rat < gc.new.tab(tag).list.len)
            return false;
    }

    return true;
}

fn scavengeTag(gc: *GC, comptime tag: wisp.Tag) !void {
    const tab = gc.new.tab(tag);

    var i = tab.rat;
    while (i < tab.list.len) : (i += 1) {
        try gc.scavengeRow(tag, tab, i);
    }

    tab.rat = i;
}

fn scavengeRow(
    gc: *GC,
    comptime tag: wisp.Tag,
    tab: *wisp.Tab(tag),
    i: wisp.Ptr.Idx,
) !void {
    inline for (std.meta.fields(wisp.Row(tag))) |_, j| {
        const col = @intToEnum(wisp.TagCol(tag), j);
        const new = try gc.copy(tab.list.items(col)[i]);
        tab.list.items(col)[i] = new;
    }
}

test "garbage collection of conses" {
    var vat = try wisp.Vat.init(std.testing.allocator, .e0);

    defer vat.deinit();

    _ = try vat.new(.duo, .{ .car = 1, .cdr = 2 });

    const cons = wisp.Row(.duo){
        .car = 3,
        .cdr = 4,
    };

    const cons1 = try vat.new(.duo, cons);

    var gc = try GC.init(&vat);
    defer gc.deinit();

    const cons2 = try gc.copy(cons1);

    try gc.scavenge();

    vat = gc.finalize();

    try std.testing.expectEqual(vat.tabs.duo.list.len, 1);
    try std.testing.expectEqual(cons, try vat.row(.duo, cons2));
}

test "read and gc" {
    var vat = try wisp.Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();

    const t1 = try read(&vat, "(foo (bar (baz)))");
    const v1 = try vat.intern("X", vat.base);

    try vat.set(.sym, .val, v1, t1);
    try tidy(&vat);

    try std.testing.expectEqual(wisp.Era.e1, vat.era);

    const v2 = try vat.intern("X", vat.base);
    const t2 = try vat.get(.sym, .val, v2);

    try printer.expect("(FOO (BAR (BAZ)))", &vat, t2);
}

test "gc ephemeral strings" {
    var vat = try wisp.Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();

    const x = try read(&vat,
        \\ ("foo" "bar" "baz")
    );

    const foo = try vat.get(.duo, .car, x);
    try vat.set(.sym, .val, try vat.intern("X", vat.base), foo);

    const n1 = vat.tabs.str.list.len;
    try tidy(&vat);
    const n2 = vat.tabs.str.list.len;
    try std.testing.expectEqual(n1 - 2, n2);
}
