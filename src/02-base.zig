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

const std = @import("std");
const assert = std.debug.assert;
const same = std.testing.expectEqual;

const wisp = @import("./ff-wisp.zig");
const ref = wisp.ref;
const nil = wisp.nil;
const Tag = wisp.Tag;
const Era = wisp.Era;
const Ptr = wisp.Ptr;

pub fn Row(comptime t: Tag) type {
    return switch (t) {
        .int, .sys, .chr, .fop, .mop => void,
        .duo => struct { car: u32, cdr: u32 },
        .sym => struct { str: u32, pkg: u32, val: u32, fun: u32 },
        .fun => struct { env: u32, exp: u32 },
        .v32 => struct { idx: u32, len: u32 },
        .v08 => struct { idx: u32, len: u32 },
        .pkg => struct { nam: u32, sym: u32 },
        .ct0 => struct { hop: u32, env: u32, fun: u32, arg: u32, exp: u32 },
        .ct1 => struct { hop: u32, env: u32, yay: u32, nay: u32 },
        .ct2 => struct { hop: u32, env: u32, exp: u32 },
        .ct3 => struct { hop: u32, env: u32, exp: u32, dew: u32, arg: u32 },
    };
}

pub const Orb = std.mem.Allocator;
pub const V08 = std.ArrayListUnmanaged(u8);
pub const V32 = std.ArrayListUnmanaged(u32);

pub const Err = error{Bad};

pub fn Col(comptime tag: Tag) type {
    return std.meta.FieldEnum(Row(tag));
}

pub fn Tab(comptime tag: Tag) type {
    return struct {
        const This = @This();
        const prefix: u32 = @enumToInt(tag) << (32 - 5);

        scan: Ptr.Idx = 0,
        list: std.MultiArrayList(Row(tag)) = .{},

        pub fn new(tab: *This, orb: Orb, era: Era, row: Row(tag)) !u32 {
            try tab.list.append(orb, row);
            const idx = @intCast(u26, tab.list.len - 1);
            return Ptr.make(tag, idx, era).word();
        }

        pub fn get(tab: This, era: Era, ptr: u32) !Row(tag) {
            const p = Ptr.from(ptr);
            assert(p.tag == tag);
            assert(p.era == era);
            return tab.list.get(p.idx);
        }

        pub fn col(tab: This, comptime c: Col(tag)) []u32 {
            return tab.list.items(c);
        }

        pub fn flip(tab: This, orb: Orb) !This {
            return This{
                .era = tab.era.flip(),
                .list = try tab.list.clone(orb),
            };
        }

        pub fn bytesize(tab: This) usize {
            return tab.list.len * @sizeOf(Row(tag));
        }
    };
}

pub const Vat = struct {
    duo: Tab(.duo) = .{},
    sym: Tab(.sym) = .{},
    fun: Tab(.fun) = .{},
    v32: Tab(.v32) = .{},
    v08: Tab(.v08) = .{},
    pkg: Tab(.pkg) = .{},
    ct0: Tab(.ct0) = .{},
    ct1: Tab(.ct1) = .{},
    ct2: Tab(.ct2) = .{},
    ct3: Tab(.ct3) = .{},

    pub fn bytesize(vat: Vat) usize {
        var n: usize = 0;
        inline for (std.meta.fields(Vat)) |field| {
            n += @field(vat, field.name).bytesize();
        }
        return n;
    }
};

fn S32(comptime t: type) type {
    return std.enums.EnumFieldStruct(t, u32, 0);
}

pub const Kwd = enum {
    IF,
    QUOTE,
    PROGN,
    @"%LET",
};

pub const Ctx = struct {
    orb: Orb,
    era: Era = .e0,
    vat: Vat = .{},
    v08: V08 = .{},
    v32: V32 = .{},
    kwd: S32(Kwd) = .{},

    base: u32 = nil,

    pub fn init(orb: Orb, era: Era) !Ctx {
        var ctx = Ctx{ .orb = orb, .era = era };

        ctx.base = try ctx.new(.pkg, .{
            .nam = try ctx.newv08("WISP"),
            .sym = nil,
        });

        inline for (std.meta.fields(Kwd)) |s| {
            @field(ctx.kwd, s.name) = try ctx.intern(s.name, ctx.base);
        }

        return ctx;
    }

    pub fn special(ctx: *Ctx, s: Kwd) u32 {
        return @field(ctx.specials, @tagName(s));
    }

    fn initvar(ctx: *Ctx, txt: []const u8, val: u32) !void {
        var sym = try ctx.intern(txt, ctx.base);
        try ctx.set(.sym, .val, sym, val);
    }

    pub fn deinit(ctx: *Ctx) void {
        ctx.v08.deinit(ctx.orb);
        ctx.v32.deinit(ctx.orb);
        inline for (std.meta.fields(Vat)) |field| {
            @field(ctx.vat, field.name).list.deinit(ctx.orb);
        }
    }

    pub fn bytesize(ctx: Ctx) usize {
        return ctx.v08.items.len + ctx.vat.bytesize();
    }

    pub fn tab(ctx: *Ctx, comptime tag: Tag) *Tab(tag) {
        return &@field(ctx.vat, @tagName(tag));
    }

    pub fn new(ctx: *Ctx, comptime tag: Tag, data: Row(tag)) !u32 {
        return ctx.tab(tag).new(ctx.orb, ctx.era, data);
    }

    pub fn row(ctx: *Ctx, comptime tag: Tag, ptr: u32) !Row(tag) {
        return ctx.tab(tag).get(ctx.era, ptr);
    }

    pub fn col(
        ctx: *Ctx,
        comptime tag: Tag,
        comptime c: Col(tag),
    ) []u32 {
        return ctx.tab(tag).col(c);
    }

    pub fn get(
        ctx: *Ctx,
        comptime tag: Tag,
        comptime c: Col(tag),
        p: u32,
    ) !u32 {
        return ctx.col(tag, c)[ref(p)];
    }

    pub fn set(
        ctx: *Ctx,
        comptime tag: Tag,
        comptime c: Col(tag),
        p: u32,
        v: u32,
    ) !void {
        ctx.col(tag, c)[ref(p)] = v;
    }

    pub fn put(
        ctx: *Ctx,
        comptime tag: Tag,
        ptr: u32,
        val: Row(tag),
    ) void {
        ctx.tab(tag).list.set(ref(ptr), val);
    }

    pub fn newv08(ctx: *Ctx, dat: []const u8) !u32 {
        try ctx.v08.appendSlice(ctx.orb, dat);
        return ctx.new(.v08, .{
            .idx = @intCast(u32, ctx.v08.items.len - dat.len),
            .len = @intCast(u32, dat.len),
        });
    }

    pub fn newv32(ctx: *Ctx, dat: []const u32) !u32 {
        try ctx.v32.appendSlice(ctx.orb, dat);
        return ctx.new(.v32, .{
            .idx = @intCast(u32, ctx.v32.items.len - dat.len),
            .len = @intCast(u32, dat.len),
        });
    }

    pub fn v08slice(ctx: *Ctx, ptr: u32) ![]const u8 {
        const str = try ctx.row(.v08, ptr);
        return ctx.v08.items[str.idx .. str.idx + str.len];
    }

    pub fn v32slice(ctx: *Ctx, ptr: u32) ![]const u32 {
        const str = try ctx.row(.v32, ptr);
        return ctx.v32.items[str.idx .. str.idx + str.len];
    }

    pub fn intern(ctx: *Ctx, txt: []const u8, pkgptr: u32) !u32 {
        if (pkgptr == ctx.base) {
            if (std.mem.eql(u8, txt, "NIL")) {
                return nil;
            } else if (std.mem.eql(u8, txt, "T")) {
                return wisp.t;
            }
        }

        const symstrs = ctx.vat.sym.list.items(.str);
        const pkg = try ctx.row(.pkg, pkgptr);
        var duoptr = pkg.sym;

        while (duoptr != nil) {
            const duo = try ctx.row(.duo, duoptr);
            const strptr = symstrs[Ptr.from(duo.car).idx];
            if (std.mem.eql(u8, txt, try ctx.v08slice(strptr))) {
                return duo.car;
            } else {
                duoptr = duo.cdr;
            }
        }

        const symptr = try ctx.new(.sym, .{
            .str = try ctx.newv08(txt),
            .pkg = pkgptr,
            .val = wisp.nah,
            .fun = nil,
        });

        try ctx.set(.pkg, .sym, pkgptr, try ctx.new(.duo, .{
            .car = symptr,
            .cdr = pkg.sym,
        }));

        return symptr;
    }
};

pub fn list(ctx: *Ctx, xs: anytype) !u32 {
    var cur = nil;
    var i: u32 = xs.len;
    while (i >= 1) : (i -= 1) {
        const x = xs[i - 1];
        cur = try ctx.new(.duo, .{ .car = x, .cdr = cur });
    }
    return cur;
}

pub fn length(ctx: *Ctx, x: u32) !u32 {
    var cur = x;
    var i: u32 = 0;
    while (cur != nil) : (i += 1) {
        cur = try ctx.get(.duo, .cdr, cur);
    }

    return i;
}

test "ctx" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();
}

test "list length" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    try same(
        @as(u32, 3),
        try length(&ctx, try list(&ctx, [_]u32{ 1, 2, 3 })),
    );
}
