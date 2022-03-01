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

old: *wisp.Ctx,
new: wisp.Ctx,

const std = @import("std");
const wisp = @import("./wisp.zig");
const read = @import("./read.zig").read;
const dump = @import("./dump.zig");

const Col = wisp.Col;
const Ctx = wisp.Ctx;
const Era = wisp.Era;
const Ptr = wisp.Ptr;
const Row = wisp.Row;
const Tab = wisp.Tab;
const Tag = wisp.Tag;

pub fn tidy(ctx: *Ctx) !void {
    const n1 = ctx.bytesize();

    var gc = try init(ctx);
    try gc.root();
    try gc.scan();
    ctx.* = gc.done();

    const n2 = ctx.bytesize();
    std.log.info("gc: before {d}, after {d}", .{ n1, n2 });
}

pub fn init(old: *Ctx) !GC {
    return GC{
        .old = old,
        .new = Ctx{
            .era = old.era.flip(),
            .orb = old.orb,
            .v08 = old.v08,
            .kwd = old.kwd,
        },
    };
}

fn done(gc: *GC) Ctx {
    gc.old.v08 = .{};
    gc.old.deinit();
    return gc.new;
}

fn root(gc: *GC) !void {
    gc.new.base = try gc.copy(gc.old.base);

    inline for (std.meta.fields(@TypeOf(gc.new.kwd))) |s| {
        try gc.move(&@field(gc.new.kwd, s.name));
    }
}

fn move(gc: *GC, x: *u32) !void {
    x.* = try gc.copy(x.*);
}

fn copy(gc: *GC, x: u32) !u32 {
    return switch (wisp.tagOf(x)) {
        .int, .chr, .sys, .fop, .mop => x,
        .sym => gc.push(.sym, x),
        .duo => gc.push(.duo, x),
        .fun => gc.push(.fun, x),
        .v32 => gc.push(.v32, x),
        .v08 => gc.push(.v08, x),
        .pkg => gc.push(.pkg, x),
        .ct0 => gc.push(.ct0, x),
        .ct1 => gc.push(.ct1, x),
    };
}

fn push(gc: *GC, comptime tag: Tag, x: u32) !u32 {
    const ptr = Ptr.from(x);
    if (ptr.era == gc.new.era) return x;

    var row = try gc.old.row(tag, x);

    var c0 = gc.old.col(tag, @intToEnum(Col(tag), 0));
    var c1 = gc.old.col(tag, @intToEnum(Col(tag), 1));
    if (c0[ptr.idx] == wisp.zap) return c1[ptr.idx];

    const new = try gc.new.new(tag, row);

    c0[ptr.idx] = wisp.zap;
    c1[ptr.idx] = new;

    return new;
}

fn scan(gc: *GC) !void {
    while (!gc.calm()) {
        inline for (wisp.pointerTags) |tag| {
            try gc.pull(tag);
        }
    }
}

fn calm(gc: *GC) bool {
    inline for (wisp.pointerTags) |tag| {
        if (gc.new.tab(tag).scan < gc.new.tab(tag).list.len)
            return false;
    }

    return true;
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
    inline for (std.meta.fields(Row(tag))) |_, j| {
        const col = @intToEnum(Col(tag), j);
        const new = try gc.copy(tab.list.items(col)[i]);
        tab.list.items(col)[i] = new;
    }
}

test "garbage collection of conses" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);

    defer ctx.deinit();

    _ = try ctx.new(.duo, .{ .car = 1, .cdr = 2 });

    const cons = Row(.duo){
        .car = 3,
        .cdr = 4,
    };

    const cons1 = try ctx.new(.duo, cons);

    var gc = try GC.init(&ctx);
    const cons2 = try gc.copy(cons1);
    try gc.scan();
    ctx = gc.done();

    try std.testing.expectEqual(ctx.vat.duo.list.len, 1);
    try std.testing.expectEqual(cons, try ctx.row(.duo, cons2));
}

test "read and gc" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    const t1 = try read(&ctx, "(foo (bar (baz)))");
    const v1 = try ctx.intern("X", ctx.base);

    try ctx.set(.sym, .val, v1, t1);
    try tidy(&ctx);

    try std.testing.expectEqual(Era.e1, ctx.era);

    const v2 = try ctx.intern("X", ctx.base);
    const t2 = try ctx.get(.sym, .val, v2);

    try dump.expect("(FOO (BAR (BAZ)))", &ctx, t2);
}

test "gc ephemeral strings" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    const x = try read(&ctx,
        \\ ("foo" "bar" "baz")
    );

    const foo = try ctx.get(.duo, .car, x);
    try ctx.set(.sym, .val, try ctx.intern("X", ctx.base), foo);

    const n1 = ctx.vat.v08.list.len;
    try tidy(&ctx);
    const n2 = ctx.vat.v08.list.len;
    try std.testing.expectEqual(n1 - 2, n2);
}
