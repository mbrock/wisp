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
const util = @import("./util.zig");

pub const W = struct {
    pub const Int = packed struct { t: u02 = 0000, v: u30 };
    pub const Duo = packed struct { t: u03 = 0001, v: u28, e: u1 };
    pub const Sym = packed struct { t: u03 = 0002, v: u28, e: u1 };
    pub const Fun = packed struct { t: u03 = 0003, v: u28, e: u1 };
    pub const Vec = packed struct { t: u03 = 0005, v: u28, e: u1 };
    pub const Str = packed struct { t: u03 = 0006, v: u28, e: u1 };
    pub const Chr = packed struct { t: u11 = 0007, v: u21 };
    pub const Pkg = packed struct { t: u11 = 0015, v: u20, e: u1 };
    pub const Sys = packed struct { t: u14 = 0023, v: u19 };
    pub const Nil = packed struct { t: u32 = 2071 };
    pub const Zap = packed struct { t: u32 = 4119 };
    pub const Ct0 = packed struct { t: u11 = 0031, v: u20, e: u1 };
};

pub const Dat = struct {
    pub const Duo = struct { car: u32, cdr: u32 };
    pub const Fun = struct { env: u32, exp: u32 };
    pub const Str = struct { idx: u32, len: u32 };
    pub const Pkg = struct { nam: u32, sym: u32 = nil };

    pub const Sym = struct {
        txt: u32,
        pkg: u32,
        val: u32 = zap,
    };

    pub const Ct0 = struct {
        hop: u32,
        env: u32,
        fun: u32,
        arg: u32 = nil,
        exp: u32 = nil,
    };
};

pub const Tag = util.DeclEnum(W, u16);

pub fn raw(w: anytype) u32 {
    return @bitCast(u32, w);
}

pub const zap = raw(W.Zap{});
pub const nil = raw(W.Nil{});

pub fn mkint(x: anytype) W.Int {
    return W.Int{ .v = @intCast(u30, x) };
}

pub fn tagword(comptime tag: Tag) type {
    return @field(W, @tagName(tag));
}

pub fn wordtag(x: u32) Tag {
    if (x == 2071) return .Nil;
    if (x == 4119) return .Zap;

    return switch (@intCast(u3, x & ~@as(u3, 0))) {
        0, 4 => .Int,
        1 => .Duo,
        2 => .Sym,
        3 => .Fun,
        5 => .Vec,
        6 => .Str,
        7 => switch (@intCast(u11, x & ~@as(u11, 0))) {
            07 => Tag.Chr,
            15 => Tag.Pkg,
            23 => Tag.Sys,
            31 => Tag.Ct0,
            else => unreachable,
        },
    };
}

pub fn tagtype(tag: Tag) type {
    return @field(W, @tagName(tag));
}

pub fn as(comptime tag: Tag, x: u32) tagtype(tag) {
    return @bitCast(tagtype(tag), x);
}

pub fn dat(comptime tag: Tag) type {
    return @field(Dat, @tagName(tag));
}

pub fn tabtype(comptime tag: Tag) type {
    return switch (tag) {
        .Duo => Tab(Dat.Duo),
        .Sym => Tab(Dat.Sym),
        .Str => Tab(Dat.Str),
        .Pkg => Tab(Dat.Pkg),
        .Ct0 => Tab(Dat.Ct0),
        else => void,
    };
}

pub const Tabs = struct {
    Duo: Tab(Dat.Duo) = .{},
    Sym: Tab(Dat.Sym) = .{},
    Str: Tab(Dat.Str) = .{},
    Pkg: Tab(Dat.Pkg) = .{},
    Ct0: Tab(Dat.Ct0) = .{},
};

pub const Era = enum(u1) { old, new };

pub fn tagval(comptime tag: Tag) type {
    const x = as(tag, 0);
    return std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(x.v)));
}

pub fn ptr(comptime tag: Tag, val: anytype, era: Era) u32 {
    return @bitCast(u32, tagword(tag){
        .v = @intCast(tagval(tag), val),
        .e = @enumToInt(era),
    });
}

pub const Orb = std.mem.Allocator;
pub const Bin = std.ArrayListUnmanaged(u8);
pub const Tab = std.MultiArrayList;

pub const Vat = struct {
    orb: Orb,
    era: Era = .new,
    bin: Bin = .{},
    tabs: Tabs = .{},

    pub fn init(orb: Orb) !Vat {
        var vat = Vat{ .orb = orb };
        try vat.tabs.Pkg.append(orb, Dat.Pkg{
            .nam = try vat.newstr("LISP"),
            .sym = nil,
        });

        return vat;
    }

    pub fn deinit(vat: *Vat) void {
        vat.bin.deinit(vat.orb);
        inline for (std.meta.fields(Tabs)) |field| {
            @field(vat.tabs, field.name).deinit(vat.orb);
        }
    }

    pub fn new(vat: *Vat, comptime tag: Tag, x: dat(tag)) !u32 {
        var tab = &@field(vat.tabs, @tagName(tag));
        try tab.append(vat.orb, x);
        return ptr(tag, 0, vat.era);
    }

    pub fn newstr(vat: *Vat, txt: []const u8) !u32 {
        const str = Dat.Str{
            .idx = raw(mkint(vat.tabs.Str.len)),
            .len = raw(mkint(txt.len)),
        };
        try vat.bin.appendSlice(vat.orb, txt);
        return vat.new(.Str, str);
    }

    pub fn get(vat: *Vat, comptime tag: Tag, ptr: u32) dat(tag) {
        const w = as(tag, ptr);
        assert(w.e == vat.era);
        return @field(vat.tabs, @tagName(tag)).get(w.v);
    }

    pub fn intern(vat: *Vat, txt: []const u8, ppkg: u32) !u32 {
        const pkg = vat.get(.Pkg, ppkg);

        var pduo = vat.tabs.Pkg.items(.sym)[pkg.v];
        const txts = vat.tabs.Sym.items(.txt);

        while (pduo != nil) {
            const duo = vat.get(.Duo, pduo);
            const txt = txts[as(.Sym, duo.car)];
        }
    }
};

test "vat" {
    var vat = try Vat.init(std.testing.allocator);
    defer vat.deinit();
}

test "foo" {
    const x: u32 = 0o7 + (123 << 11);
    const chr = @bitCast(W.Chr, x);
    try std.testing.expectEqual(Tag.Chr, wordtag(x));
    try std.testing.expectEqual(0o7, chr.t);
    try std.testing.expectEqual(123, chr.v);
    try std.testing.expectEqual(0o4027, @bitCast(u32, W.Nil{}));

    const fun = W.Fun{ .v = 1, .e = 1 };
    try std.testing.expectEqual(
        0b10000000000000000000000000001011,
        @bitCast(u32, fun),
    );

    const fun2 = as(.Fun, 0b10000000000000000000000000001011);
    try std.testing.expectEqual(fun, fun2);
}
