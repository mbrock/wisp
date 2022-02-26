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

pub const Orb = std.mem.Allocator;
pub const Bin = std.ArrayListUnmanaged(u8);
pub const Tab = std.MultiArrayList;

pub const W = struct {
    pub const Int = packed struct { tag: u02 = 0o00000, val: u30 };
    pub const Duo = packed struct { tag: u03 = 0o00001, val: u28, era: u1 };
    pub const Sym = packed struct { tag: u03 = 0o00002, val: u28, era: u1 };
    pub const Fun = packed struct { tag: u03 = 0o00003, val: u28, era: u1 };
    pub const Vec = packed struct { tag: u03 = 0o00005, val: u28, era: u1 };
    pub const Str = packed struct { tag: u03 = 0o00006, val: u28, era: u1 };
    pub const Chr = packed struct { tag: u11 = 0o00007, val: u21 };
    pub const Pkg = packed struct { tag: u11 = 0o00017, val: u20, era: u1 };
    pub const Sys = packed struct { tag: u14 = 0o00027, val: u19 };
    pub const Nil = packed struct { tag: u32 = 0o04027 };
    pub const Zap = packed struct { tag: u32 = 0o10027 };
    pub const Ct0 = packed struct { tag: u11 = 0o00037, val: u20, era: u1 };
};

pub const zap = @bitCast(u32, W.Zap{});
pub const nil = @bitCast(u32, W.Nil{});

pub const Dat = struct {
    pub const Duo = struct { car: u32, cdr: u32 };
    pub const Sym = struct { txt: u32, pkg: u32, val: u32, fun: u32 };
    pub const Fun = struct { ctx: u32, exp: u32 };
    pub const Str = struct { idx: u32, len: u32 };
    pub const Pkg = struct { who: u32, sym: u32 };
    pub const Ct0 = struct { hop: u32, ctx: u32, fun: u32, val: u32, exp: u32 };
};

pub const Tag = @import("./util.zig").DeclEnum(W, u16);

pub fn tagword(comptime tag: Tag) type {
    return @field(W, @tagName(tag));
}

pub fn wordtag(x: u32) Tag {
    if (x == 0o04027) return .Nil;
    if (x == 010027) return .Zap;

    return switch (@intCast(u3, x & ~@as(u3, 0))) {
        0 => .Int,
        1 => .Duo,
        2 => .Sym,
        3 => .Fun,
        4 => unreachable,
        5 => .Vec,
        6 => .Str,
        7 => switch (@intCast(u11, x & ~@as(u11, 0))) {
            0o00007 => Tag.Chr,
            0o00017 => Tag.Pkg,
            0o00027 => Tag.Sys,
            0o00030 => Tag.Ct0,
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

test "foo" {
    const x: u32 = 0o7 + (123 << 11);
    const chr = @bitCast(W.Chr, x);
    try std.testing.expectEqual(Tag.Chr, wordtag(x));
    try std.testing.expectEqual(0o7, chr.tag);
    try std.testing.expectEqual(123, chr.val);
    try std.testing.expectEqual(0o4027, @bitCast(u32, W.Nil{}));

    const fun = W.Fun{ .val = 1, .era = 1 };
    try std.testing.expectEqual(
        0b10000000000000000000000000001011,
        @bitCast(u32, fun),
    );

    const fun2 = as(.Fun, 0b10000000000000000000000000001011);
    try std.testing.expectEqual(fun, fun2);
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
    return std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(x.val)));
}

pub fn ptr(comptime tag: Tag, val: anytype, era: Era) u32 {
    return @bitCast(u32, tagword(tag){
        .val = @intCast(tagval(tag), val),
        .era = @enumToInt(era),
    });
}

pub const Vat = struct {
    orb: Orb,
    era: Era = .new,
    bin: Bin = .{},
    tabs: Tabs = .{},

    pub fn init(orb: Orb) !Vat {
        var vat = Vat{ .orb = orb };
        try vat.tabs.Pkg.append(orb, Dat.Pkg{
            .who = try vat.newstr("LISP"),
            .sym = nil,
        });

        return vat;
    }

    pub fn new(vat: *Vat, comptime tag: Tag, x: dat(tag)) !u32 {
        var tab = @field(vat.tabs, @tagName(tag));
        try tab.append(vat.orb, x);
        return ptr(tag, 0, vat.era);
    }

    pub fn newstr(vat: *Vat, txt: []const u8) !u32 {
        const str = Dat.Str{
            .idx = @intCast(u32, vat.tabs.Str.len),
            .len = @intCast(u32, txt.len),
        };
        try vat.bin.appendSlice(vat.orb, txt);
        return vat.new(.Str, str);
    }
};

test "vat" {
    var vat = try Vat.init(std.testing.allocator);
    defer vat.bin.deinit(std.testing.allocator);
    defer vat.tabs.Str.deinit(vat.orb);
}
