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

const GC = @This();

old: *wisp.Heap,
new: wisp.Heap,

const std = @import("std");

const wisp = @import("./ff-wisp.zig");
const Eval = @import("./04-eval.zig");
const read = @import("./05-read.zig").read;
const dump = @import("./06-dump.zig");

const Heap = wisp.Heap;
const Col = wisp.Col;
const Era = wisp.Era;
const Ptr = wisp.Ptr;
const Row = wisp.Row;
const Tab = wisp.Tab;
const Tag = wisp.Tag;

pub fn tidy(heap: *Heap) !void {
    // const n1 = heap.bytesize();

    var gc = try init(heap);
    try gc.root();
    try gc.scan();
    heap.* = gc.done();

    // const n2 = heap.bytesize();
    // std.log.info("gc: before {d}, after {d}", .{ n1, n2 });
}

pub fn tidyEval(eval: *Eval) !void {
    var gc = try init(eval.heap);
    try gc.root();

    try gc.move(&eval.bot.err);
    try gc.move(&eval.bot.env);
    try gc.move(&eval.bot.way);
    try gc.move(&eval.bot.val);
    try gc.move(&eval.bot.exp);

    try gc.scan();
    eval.heap.* = gc.done();
}

pub fn init(old: *Heap) !GC {
    return GC{
        .old = old,
        .new = Heap{
            .era = old.era.flip(),
            .orb = old.orb,
            .v08 = old.v08,
            .kwd = old.kwd,
            .base = old.base,
            .keywordPackage = old.keywordPackage,
            .pkg = old.pkg,
        },
    };
}

fn done(gc: *GC) Heap {
    gc.old.v08 = .{};
    gc.old.deinit();
    return gc.new;
}

fn root(gc: *GC) !void {
    try gc.move(&gc.new.base);
    try gc.move(&gc.new.keywordPackage);
    try gc.move(&gc.new.pkg);

    inline for (std.meta.fields(@TypeOf(gc.new.kwd))) |s| {
        try gc.move(&@field(gc.new.kwd, s.name));
    }
}

/// We use an `anytype` because the pointer might have some
/// specific alignment.
fn move(gc: *GC, x: anytype) !void {
    x.* = try gc.copy(x.*);
}

fn copy(gc: *GC, x: u32) !u32 {
    return switch (wisp.tagOf(x)) {
        .int, .chr, .sys, .jet => x,
        .sym => gc.push(.sym, x),
        .duo => gc.push(.duo, x),
        .fun => gc.push(.fun, x),
        .mac => gc.push(.mac, x),
        .v32 => gc.push(.v32, x),
        .v08 => gc.push(.v08, x),
        .pkg => gc.push(.pkg, x),
        .ktx => gc.push(.ktx, x),
        .bot => gc.push(.bot, x),
    };
}

fn push(gc: *GC, comptime tag: Tag, x: u32) !u32 {
    const ptr = Ptr.from(x);
    if (ptr.era == gc.new.era) return x;

    var c0 = gc.old.col(tag, @intToEnum(Col(tag), 0));
    var c1 = gc.old.col(tag, @intToEnum(Col(tag), 1));
    if (c0[ptr.idx] == wisp.zap) return c1[ptr.idx];

    const new = if (tag == .v32)
        try gc.new.newv32(try gc.old.v32slice(x))
    else
        try gc.new.new(tag, try gc.old.row(tag, x));

    c0[ptr.idx] = wisp.zap;
    c1[ptr.idx] = new;

    return new;
}

fn scan(gc: *GC) !void {
    while (!gc.calm()) {
        inline for (wisp.pointerTags) |tag| {
            try gc.pull(tag);
        }

        try gc.pullV32();
    }
}

fn calm(gc: *GC) bool {
    var r = true;

    inline for (wisp.pointerTags) |tag| {
        if (gc.new.tab(tag).scan < gc.new.tab(tag).list.len) {
            r = false;
        }
    }

    if (gc.new.v32.scan < gc.new.v32.list.items.len)
        r = false;

    return r;
}

fn pullV32(gc: *GC) !void {
    const tab = &gc.new.v32;

    var i = tab.scan;
    while (i < tab.list.items.len) : (i += 1) {
        try gc.move(&tab.list.items[i]);
    }

    tab.scan = i;
}

fn pull(gc: *GC, comptime tag: Tag) !void {
    const tab = gc.new.tab(tag);

    var i = tab.scan;
    while (i < tab.list.len) : (i += 1) {
        try gc.drag(tag, tab, i);
    }

    tab.scan = i;
}

fn drag(gc: *GC, comptime tag: Tag, tab: *Tab(tag), i: Ptr.Idx) !void {
    inline for (std.meta.fields(Col(tag))) |_, j| {
        const col = @intToEnum(Col(tag), j);
        const new = try gc.copy(tab.list.items(col)[i]);
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

    var gc = try GC.init(&heap);
    const cons2 = try gc.copy(cons1);
    try gc.scan();
    heap = gc.done();

    try std.testing.expectEqual(heap.vat.duo.list.len, 1);
    try std.testing.expectEqual(cons, try heap.row(.duo, cons2));
}

test "read and gc" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    const t1 = try read(&heap, "(foo (bar (baz)))");
    const v1 = try heap.intern("X", heap.base);

    try heap.set(.sym, .val, v1, t1);
    try tidy(&heap);

    try std.testing.expectEqual(Era.e1, heap.era);

    const v2 = try heap.intern("X", heap.base);
    const t2 = try heap.get(.sym, .val, v2);

    try dump.expect("(FOO (BAR (BAZ)))", &heap, t2);
}

test "gc ephemeral strings" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    const x = try read(&heap,
        \\ ("foo" "bar" "baz")
    );

    const foo = try heap.get(.duo, .car, x);
    try heap.set(.sym, .val, try heap.intern("X", heap.base), foo);

    const n1 = heap.vat.v08.list.len;
    try tidy(&heap);
    const n2 = heap.vat.v08.list.len;
    try std.testing.expectEqual(n1 - 2, n2);
}
