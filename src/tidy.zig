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

const Tidy = @This();

old: *Wisp.Heap,
new: Wisp.Heap,

const std = @import("std");

const Wisp = @import("./wisp.zig");
const Step = @import("./step.zig");
const Sexp = @import("./sexp.zig");

const Heap = Wisp.Heap;
const Col = Wisp.Col;
const Era = Wisp.Era;
const Ptr = Wisp.Ptr;
const Row = Wisp.Row;
const Tab = Wisp.Tab;
const Tag = Wisp.Tag;

pub fn gc(heap: *Heap) !void {
    var this = try init(heap);
    try this.root();
    try this.scan();
    heap.* = this.done();
}

pub fn init(old: *Heap) !Tidy {
    return Tidy{
        .old = old,
        .new = Heap{
            .era = old.era.flip(),
            .orb = old.orb,
            .v08 = old.v08,
            .kwd = old.kwd,
            .base = old.base,
            .keywordPackage = old.keywordPackage,
            .keyPackage = old.keyPackage,
            .pkg = old.pkg,
        },
    };
}

pub fn done(tidy: *Tidy) Heap {
    tidy.old.v08 = .{};
    tidy.old.deinit();
    return tidy.new;
}

pub fn root(tidy: *Tidy) !void {
    try tidy.move(&tidy.new.base);
    try tidy.move(&tidy.new.keywordPackage);
    try tidy.move(&tidy.new.keyPackage);
    try tidy.move(&tidy.new.pkg);

    inline for (std.meta.fields(@TypeOf(tidy.new.kwd))) |s| {
        try tidy.move(&@field(tidy.new.kwd, s.name));
    }
}

/// We use an `anytype` because the pointer might have some
/// specific alignment.
pub fn move(tidy: *Tidy, x: anytype) !void {
    x.* = try tidy.copy(x.*);
}

fn copy(tidy: *Tidy, x: u32) !u32 {
    return switch (Wisp.tagOf(x)) {
        .int, .chr, .sys, .jet => x,
        .sym => tidy.push(.sym, x),
        .duo => tidy.push(.duo, x),
        .fun => tidy.push(.fun, x),
        .mac => tidy.push(.mac, x),
        .v32 => tidy.push(.v32, x),
        .v08 => tidy.push(.v08, x),
        .pkg => tidy.push(.pkg, x),
        .ktx => tidy.push(.ktx, x),
        .run => tidy.push(.run, x),
    };
}

fn push(tidy: *Tidy, comptime tag: Tag, x: u32) !u32 {
    const ptr = Ptr.from(x);
    if (ptr.era == tidy.new.era) return x;

    var c0 = tidy.old.col(tag, @intToEnum(Col(tag), 0));
    var c1 = tidy.old.col(tag, @intToEnum(Col(tag), 1));
    if (c0[ptr.idx] == Wisp.zap) return c1[ptr.idx];

    const new = if (tag == .v32)
        try tidy.new.newv32(try tidy.old.v32slice(x))
    else
        try tidy.new.new(tag, try tidy.old.row(tag, x));

    c0[ptr.idx] = Wisp.zap;
    c1[ptr.idx] = new;

    return new;
}

pub fn scan(tidy: *Tidy) !void {
    while (!tidy.calm()) {
        inline for (Wisp.pointerTags) |tag| {
            try tidy.pull(tag);
        }

        try tidy.pullV32();
    }
}

fn calm(tidy: *Tidy) bool {
    var r = true;

    inline for (Wisp.pointerTags) |tag| {
        if (tidy.new.tab(tag).scan < tidy.new.tab(tag).list.len) {
            r = false;
        }
    }

    if (tidy.new.v32.scan < tidy.new.v32.list.items.len)
        r = false;

    return r;
}

fn pullV32(tidy: *Tidy) !void {
    const tab = &tidy.new.v32;

    var i = tab.scan;
    while (i < tab.list.items.len) : (i += 1) {
        try tidy.move(&tab.list.items[i]);
    }

    tab.scan = i;
}

fn pull(tidy: *Tidy, comptime tag: Tag) !void {
    const tab = tidy.new.tab(tag);

    var i = tab.scan;
    while (i < tab.list.len) : (i += 1) {
        try tidy.drag(tag, tab, i);
    }

    tab.scan = i;
}

fn drag(tidy: *Tidy, comptime tag: Tag, tab: *Tab(tag), i: Ptr.Idx) !void {
    inline for (std.meta.fields(Col(tag))) |_, j| {
        const col = @intToEnum(Col(tag), j);
        const new = try tidy.copy(tab.list.items(col)[i]);
        tab.list.items(col)[i] = new;
    }
}

test "garbage collection of conses" {
    var heap = try Heap.init(std.testing.allocator, .e0);

    defer heap.deinit();

    _ = try heap.new(.duo, .{ .car = 1, .cdr = 2 });

    const cons = Row(.duo){
        .car = 3,
        .cdr = 4,
    };

    const cons1 = try heap.new(.duo, cons);

    var tidy = try Tidy.init(&heap);
    const cons2 = try tidy.copy(cons1);
    try tidy.scan();
    heap = tidy.done();

    try std.testing.expectEqual(heap.vat.duo.list.len, 1);
    try std.testing.expectEqual(cons, try heap.row(.duo, cons2));
}

test "read and tidy" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    const t1 = try Sexp.read(&heap, "(foo (bar (baz)))");
    const v1 = try heap.intern("X", heap.base);

    try heap.set(.sym, .val, v1, t1);
    try gc(&heap);

    try std.testing.expectEqual(Era.e1, heap.era);

    const v2 = try heap.intern("X", heap.base);
    const t2 = try heap.get(.sym, .val, v2);

    try Sexp.expectDump("(FOO (BAR (BAZ)))", &heap, t2);
}

test "tidy ephemeral strings" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    const x = try Sexp.read(&heap,
        \\ ("foo" "bar" "baz")
    );

    const foo = try heap.get(.duo, .car, x);
    try heap.set(.sym, .val, try heap.intern("X", heap.base), foo);

    const n1 = heap.vat.v08.list.len;
    try gc(&heap);
    const n2 = heap.vat.v08.list.len;

    try std.testing.expectEqual(n1 - 2, n2);
}
