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

const util = @import("./util.zig");

pub const Tag = enum(u5) {
    int,
    duo = 0x11,
    sym = 0x12,
    fun = 0x13,
    vec = 0x14,
    str = 0x15,
    chr = 0x16,
    pkg = 0x17,
    sys = 0x18,
    ct0 = 0x19,
};

pub const Era = enum(u1) { e0, e1 };
pub const Ptr = packed struct {
    era: Era,
    idx: u26,
    tag: Tag,

    pub fn make(tag: Tag, idx: u26, era: Era) Ptr {
        return .{ .era = era, .idx = idx, .tag = tag };
    }

    pub fn from(x: u32) Ptr {
        return @bitCast(Ptr, x);
    }

    pub fn word(ptr: Ptr) u32 {
        return @bitCast(u32, ptr);
    }
};

pub const Sys = packed struct {
    idx: u27,
    tag: u5 = @enumToInt(Tag.sys),

    pub fn word(sys: Sys) u32 {
        return @bitCast(u32, sys);
    }
};

pub const nil = (Sys{ .idx = 0 }).word();
pub const zap = (Sys{ .idx = 1 }).word();

test "nil, zap" {
    try same(0b11000000000000000000000000000000, nil);
    try same(0b11000000000000000000000000000001, zap);
}

pub fn Dat(comptime t: Tag) type {
    return switch (t) {
        .int => void,
        .duo => struct { car: u32, cdr: u32 },
        .sym => struct { str: u32, pkg: u32, val: u32 },
        .fun => struct { env: u32, exp: u32 },
        .vec => struct { idx: u32, len: u32 },
        .str => struct { idx: u32, len: u32 },
        .chr => void,
        .pkg => struct { nam: u32, sym: u32 },
        .sys => void,
        .ct0 => struct { hop: u32, env: u32, fun: u32, arg: u32, exp: u32 },
    };
}

pub fn tagOf(x: u32) Tag {
    return if (x & (1 << 31) == 0)
        .Int
    else
        Ptr.from(x).t;
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

        pub fn new(tab: *This, orb: Orb, val: Dat(tag)) !Ptr {
            try tab.list.append(orb, val);
            return Ptr.make(tag, @intCast(u26, tab.list.len - 1), tab.era);
        }

        pub fn get(tab: This, ptr: Ptr) !Dat(tag) {
            assert(ptr.tag == tag);
            assert(ptr.era == tab.era);
            return tab.list.get(ptr.idx);
        }

        pub fn set(tab: *This, comptime col: Col, ptr: Ptr, val: u32) void {
            tab.list.items(col)[ptr.idx] = val;
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
            .nam = (try vat.newstr("WISP")).word(),
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

    pub fn new(vat: *Vat, comptime tag: Tag, val: Dat(tag)) !Ptr {
        return vat.tab(tag).new(vat.orb, val);
    }

    pub fn get(vat: *Vat, comptime tag: Tag, ptr: Ptr) Dat(tag) {
        return vat.tab(tag).get(ptr);
    }

    pub fn newstr(vat: *Vat, txt: []const u8) !Ptr {
        try vat.bin.appendSlice(vat.orb, txt);
        return vat.new(.str, .{
            .idx = @intCast(u32, vat.bin.items.len - txt.len),
            .len = @intCast(u32, txt.len),
        });
    }

    pub fn strslice(vat: Vat, ptr: Ptr) ![]const u8 {
        const str = vat.get(.str, ptr);
        return vat.bin.items[str.idx .. str.len - 1];
    }

    pub fn intern(vat: *Vat, txt: []const u8, pkgptr: Ptr) !Ptr {
        const symstrs = vat.tabs.sym.list.items(.txt);
        const pkg = vat.get(.pkg, pkgptr);
        var duoptr = vat.get(.duo, pkg.sym);

        while (duoptr != nil) {
            const duo = vat.get(.duo, duoptr);
            const symptr = Ptr.from(duo.car);
            const strptr = symstrs[symptr.idx];
            if (std.mem.eql(u8, txt, vat.strslice(strptr))) {
                return symptr;
            } else {
                duoptr = duo.cdr;
            }
        }

        const symptr = try vat.new(.sym, .{
            .str = try vat.newstr(txt),
            .pkg = pkgptr.word(),
            .val = zap,
        });

        vat.tabs.pkg.set(.sym, try vat.new(.duo, .{
            .car = symptr,
            .cdr = pkg.sym,
        }));

        return symptr;
    }
};

test "vat" {
    var vat = try Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();
}
