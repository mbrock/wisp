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

const wisp = @import("./wisp.zig");
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
        .vec => struct { idx: u32, len: u32 },
        .str => struct { idx: u32, len: u32 },
        .pkg => struct { nam: u32, sym: u32 },
        .ct0 => struct { hop: u32, env: u32, fun: u32, arg: u32, exp: u32 },
        .ct1 => struct { hop: u32, env: u32, yay: u32, nay: u32 },
    };
}

pub const Orb = std.mem.Allocator;
pub const Bin = std.ArrayListUnmanaged(u8);

pub const Err = error{Bad};

pub fn TagCol(comptime tag: Tag) type {
    return std.meta.FieldEnum(Row(tag));
}

pub fn Tab(comptime tag: Tag) type {
    return struct {
        const This = @This();

        era: Era,
        rat: Ptr.Idx = 0,

        list: std.MultiArrayList(Row(tag)) = .{},

        const prefix: u32 = @enumToInt(tag) << (32 - 5);

        pub fn new(tab: *This, orb: Orb, row: Row(tag)) !u32 {
            try tab.list.append(orb, row);
            return Ptr.make(
                tag,
                @intCast(u26, tab.list.len - 1),
                tab.era,
            ).word();
        }

        pub fn makeptr(tab: This, idx: u26) Ptr {
            return Ptr.make(tag, idx, tab.era);
        }

        pub fn get(tab: This, ptr: u32) !Row(tag) {
            const p = Ptr.from(ptr);
            assert(p.tag == tag);
            assert(p.era == tab.era);
            return tab.list.get(p.idx);
        }

        pub fn col(tab: This, comptime c: TagCol(tag)) []u32 {
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

pub const Tabs = struct {
    duo: Tab(.duo),
    sym: Tab(.sym),
    fun: Tab(.fun),
    vec: Tab(.vec),
    str: Tab(.str),
    pkg: Tab(.pkg),
    ct0: Tab(.ct0),
    ct1: Tab(.ct1),

    pub fn init(era: Era) Tabs {
        return .{
            .duo = .{ .era = era },
            .sym = .{ .era = era },
            .fun = .{ .era = era },
            .vec = .{ .era = era },
            .str = .{ .era = era },
            .pkg = .{ .era = era },
            .ct0 = .{ .era = era },
            .ct1 = .{ .era = era },
        };
    }

    pub fn bytesize(tabs: Tabs) usize {
        var n: usize = 0;
        inline for (std.meta.fields(Tabs)) |field| {
            n += @field(tabs, field.name).bytesize();
        }
        return n;
    }
};

pub const Special = enum {
    NIL,
    T,
    IF,
};

pub const Vat = struct {
    orb: Orb,
    era: Era = .e0,
    bin: Bin = .{},
    tabs: Tabs,

    base: u32,

    specials: std.enums.EnumFieldStruct(Special, u32, 0) = .{},

    pub fn init(orb: Orb, era: Era) !Vat {
        var vat = Vat{
            .orb = orb,
            .tabs = Tabs.init(era),
            .base = 0xdeadbeef,
        };

        vat.base = try vat.new(.pkg, .{
            .nam = try vat.newstr("WISP"),
            .sym = nil,
        });

        try vat.initvar("NIL", wisp.nil);
        try vat.initvar("T", wisp.t);

        inline for (std.meta.fields(Special)) |s| {
            @field(vat.specials, s.name) = try vat.intern(s.name, vat.base);
        }

        return vat;
    }

    pub fn special(vat: *Vat, s: Special) u32 {
        return @field(vat.specials, @tagName(s));
    }

    fn initvar(vat: *Vat, txt: []const u8, val: u32) !void {
        var sym = try vat.intern(txt, vat.base);
        try vat.set(.sym, .val, sym, val);
    }

    pub fn deinit(vat: *Vat) void {
        vat.bin.deinit(vat.orb);
        inline for (std.meta.fields(Tabs)) |field| {
            @field(vat.tabs, field.name).list.deinit(vat.orb);
        }
    }

    pub fn bytesize(vat: Vat) usize {
        return vat.bin.items.len + vat.tabs.bytesize();
    }

    pub fn tab(vat: *Vat, comptime tag: Tag) *Tab(tag) {
        return &@field(vat.tabs, @tagName(tag));
    }

    pub fn new(vat: *Vat, comptime tag: Tag, data: Row(tag)) !u32 {
        return vat.tab(tag).new(vat.orb, data);
    }

    pub fn row(vat: *Vat, comptime tag: Tag, ptr: u32) !Row(tag) {
        return vat.tab(tag).get(ptr);
    }

    pub fn col(
        vat: *Vat,
        comptime tag: Tag,
        comptime c: TagCol(tag),
    ) []u32 {
        return vat.tab(tag).col(c);
    }

    pub fn get(
        vat: *Vat,
        comptime tag: Tag,
        comptime c: TagCol(tag),
        p: u32,
    ) !u32 {
        return vat.col(tag, c)[ref(p)];
    }

    pub fn put(
        vat: *Vat,
        comptime tag: Tag,
        ptr: u32,
        val: Row(tag),
    ) void {
        vat.tab(tag).list.set(ref(ptr), val);
    }

    pub fn set(
        vat: *Vat,
        comptime tag: Tag,
        comptime c: TagCol(tag),
        p: u32,
        v: u32,
    ) !void {
        vat.col(tag, c)[ref(p)] = v;
    }

    pub fn newstr(vat: *Vat, txt: []const u8) !u32 {
        try vat.bin.appendSlice(vat.orb, txt);
        return vat.new(.str, .{
            .idx = @intCast(u32, vat.bin.items.len - txt.len),
            .len = @intCast(u32, txt.len),
        });
    }

    pub fn strslice(vat: *Vat, ptr: u32) ![]const u8 {
        const str = try vat.row(.str, ptr);
        return vat.bin.items[str.idx .. str.idx + str.len];
    }

    pub fn intern(vat: *Vat, txt: []const u8, pkgptr: u32) !u32 {
        const symstrs = vat.tabs.sym.list.items(.str);
        const pkg = try vat.row(.pkg, pkgptr);
        var duoptr = pkg.sym;

        while (duoptr != nil) {
            const duo = try vat.row(.duo, duoptr);
            const strptr = symstrs[Ptr.from(duo.car).idx];
            if (std.mem.eql(u8, txt, try vat.strslice(strptr))) {
                return duo.car;
            } else {
                duoptr = duo.cdr;
            }
        }

        const symptr = try vat.new(.sym, .{
            .str = try vat.newstr(txt),
            .pkg = pkgptr,
            .val = wisp.nah,
            .fun = nil,
        });

        vat.col(.pkg, .sym)[ref(pkgptr)] = try vat.new(.duo, .{
            .car = symptr,
            .cdr = pkg.sym,
        });

        return symptr;
    }
};

pub fn list(vat: *Vat, xs: anytype) !u32 {
    var cur = nil;
    var i = xs.len;
    while (i >= 1) : (i -= 1) {
        const x = xs[i - 1];
        cur = try vat.new(.duo, .{ .car = x, .cdr = cur });
    }
    return cur;
}

test "vat" {
    var vat = try Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();
}
