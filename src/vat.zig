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
const nil = wisp.nil;
const zap = wisp.zap;
const Tag = wisp.Tag;
const Era = wisp.Era;
const Ptr = wisp.Ptr;

pub fn Dat(comptime t: Tag) type {
    return switch (t) {
        .int, .sys, .chr, .fop, .mop => void,
        .duo => struct { car: u32, cdr: u32 },
        .sym => struct { str: u32, pkg: u32, val: u32, fun: u32 },
        .fun => struct { env: u32, exp: u32 },
        .vec => struct { idx: u32, len: u32 },
        .str => struct { idx: u32, len: u32 },
        .pkg => struct { nam: u32, sym: u32 },
        .ct0 => struct { hop: u32, env: u32, fun: u32, arg: u32, exp: u32 },
    };
}

pub const Orb = std.mem.Allocator;
pub const Bin = std.ArrayListUnmanaged(u8);

pub const Err = error{Bad};

pub fn Tab(comptime tag: Tag) type {
    return struct {
        const This = @This();

        era: Era,
        list: std.MultiArrayList(Dat(tag)) = .{},

        const Col = std.meta.FieldEnum(Dat(tag));
        const prefix: u32 = @enumToInt(tag) << (32 - 5);

        pub fn new(tab: *This, orb: Orb, val: Dat(tag)) !u32 {
            try tab.list.append(orb, val);
            return Ptr.make(
                tag,
                @intCast(u26, tab.list.len - 1),
                tab.era,
            ).word();
        }

        pub fn makeptr(tab: This, idx: u26) Ptr {
            return Ptr.make(tag, idx, tab.era);
        }

        pub fn get(tab: This, ptr: u32) !Dat(tag) {
            const p = Ptr.from(ptr);
            assert(p.tag == tag);
            assert(p.era == tab.era);
            return tab.list.get(p.idx);
        }

        pub fn field(tab: This, comptime col: Col) []u32 {
            return tab.list.items(col);
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

    pub fn init(era: Era) Tabs {
        return .{
            .duo = .{ .era = era },
            .sym = .{ .era = era },
            .fun = .{ .era = era },
            .vec = .{ .era = era },
            .str = .{ .era = era },
            .pkg = .{ .era = era },
            .ct0 = .{ .era = era },
        };
    }
};

pub const Vat = struct {
    orb: Orb,
    era: Era = .e0,
    bin: Bin = .{},
    tabs: Tabs,

    pub fn init(orb: Orb, era: Era) !Vat {
        var vat = Vat{
            .orb = orb,
            .tabs = Tabs.init(era),
        };

        _ = try vat.new(.pkg, .{
            .nam = try vat.newstr("WISP"),
            .sym = nil,
        });

        return vat;
    }

    pub fn deinit(vat: *Vat) void {
        vat.bin.deinit(vat.orb);
        inline for (std.meta.fields(Tabs)) |field| {
            @field(vat.tabs, field.name).list.deinit(vat.orb);
        }
    }

    pub fn tab(vat: *Vat, comptime tag: Tag) *Tab(tag) {
        return &@field(vat.tabs, @tagName(tag));
    }

    pub fn new(vat: *Vat, comptime tag: Tag, val: Dat(tag)) !u32 {
        return vat.tab(tag).new(vat.orb, val);
    }

    pub fn get(vat: *Vat, comptime tag: Tag, ptr: u32) !Dat(tag) {
        return vat.tab(tag).get(ptr);
    }

    pub fn newstr(vat: *Vat, txt: []const u8) !u32 {
        try vat.bin.appendSlice(vat.orb, txt);
        return vat.new(.str, .{
            .idx = @intCast(u32, vat.bin.items.len - txt.len),
            .len = @intCast(u32, txt.len),
        });
    }

    pub fn strslice(vat: *Vat, ptr: u32) ![]const u8 {
        const str = try vat.get(.str, ptr);
        return vat.bin.items[str.idx .. str.idx + str.len];
    }

    pub fn base(vat: Vat) u32 {
        return vat.tabs.pkg.makeptr(0).word();
    }

    pub fn intern(vat: *Vat, txt: []const u8, pkgptr: u32) !u32 {
        const symstrs = vat.tabs.sym.list.items(.str);
        const pkg = try vat.get(.pkg, pkgptr);
        var duoptr = pkg.sym;

        while (duoptr != nil) {
            const duo = try vat.get(.duo, duoptr);
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
            .val = zap,
            .fun = nil,
        });

        vat.tabs.pkg.field(.sym)[Ptr.from(pkgptr).idx] = try vat.new(.duo, .{
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
