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

const std = @import("std");
const assert = std.debug.assert;
const same = std.testing.expectEqual;

const tidy = @import("./03-tidy.zig");
const eval = @import("./04-eval.zig");
const read = @import("./05-read.zig");
const dump = @import("./06-dump.zig");
const wisp = @import("./ff-wisp.zig");

const ref = wisp.ref;
const nil = wisp.nil;
const nah = wisp.nah;
const Tag = wisp.Tag;
const Era = wisp.Era;
const Ptr = wisp.Ptr;

/// Each pointer type has a set of columns.
pub fn ColEnum(comptime t: Tag) type {
    return switch (t) {
        .int, .sys, .chr, .jet => void,
        .duo => enum { car, cdr },
        .sym => enum { str, pkg, val, fun },
        .fun => enum { env, par, exp },
        .mac => enum { env, par, exp },
        .v08 => enum { idx, len },
        .v32 => enum { idx, len },
        .pkg => enum { nam, sym, use },
        .ct0 => enum { env, fun, arg, exp, hop },
        .ct1 => enum { env, yay, nay, hop },
        .ct2 => enum { env, exp, hop },
        .ct3 => enum { env, exp, dew, arg, hop },
    };
}

/// These symbols are always interned and available for easy access
/// from the Zig code.
pub const Kwd = enum {
    QUOTE,
    QUASIQUOTE,
    UNQUOTE,

    @"&REST",

    BOOLEAN,
    CHARACTER,
    CONS,
    CONTINUATION,
    FUNCTION,
    INTEGER,
    MACRO,
    NULL,
    PACKAGE,
    STRING,
    SYMBOL,
    VECTOR,

    @"BUG",
    @"EXHAUSTED",
    @"PACKAGE-ERROR",
    @"PROGRAM-ERROR",
    @"UNBOUND-VARIABLE",
    @"UNDEFINED-FUNCTION",
    @"TYPE-MISMATCH",
};

/// The orb is the vat's allocator.  All Lisp values reside in memory
/// granted from the orb.
pub const Orb = std.mem.Allocator;

/// A vat uses a single growing byte array for all its string data.
pub const V08 = std.ArrayListUnmanaged(u8);

/// A vat uses a single growing word array for all its vector data.
pub const V32 = struct {
    list: std.ArrayListUnmanaged(u32) = .{},
    scan: u32 = 0,
};

/// Zig errors are used for escaping from errors in the evaluator, but
/// they're just enums.  Lisp conditions are rows in the vat.  So the
/// step function can always fail, and when it fails, it stores its
/// err in the job.
pub const Oof = error{ Ugh, Bug, Err };

pub fn Row(comptime tag: Tag) type {
    return std.enums.EnumFieldStruct(ColEnum(tag), u32, null);
}

pub fn Col(comptime tag: Tag) type {
    return Tab(tag).Field;
}

pub fn Tab(comptime tag: Tag) type {
    return struct {
        const This = @This();
        const prefix: u32 = @enumToInt(tag) << (32 - 5);

        pub const Field = std.MultiArrayList(Row(tag)).Field;

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
    mac: Tab(.mac) = .{},
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

pub const Ctx = struct {
    orb: Orb,
    era: Era = .e0,
    vat: Vat = .{},
    v08: V08 = .{},
    v32: V32 = .{},
    kwd: S32(Kwd) = .{},

    base: u32 = nil,
    keywordPackage: u32 = nil,

    pkg: u32 = nil,
    pkgmap: std.StringArrayHashMapUnmanaged(u32) = .{},

    pub fn init(orb: Orb, era: Era) !Ctx {
        var ctx = Ctx{ .orb = orb, .era = era };

        ctx.base = try ctx.defpackage(try ctx.newv08("WISP"), nil);
        ctx.keywordPackage = try ctx.defpackage(try ctx.newv08("KEYWORD"), nil);
        ctx.pkg = ctx.base;

        inline for (std.meta.fields(Kwd)) |s| {
            @field(ctx.kwd, s.name) = try ctx.intern(s.name, ctx.base);
        }

        return ctx;
    }

    pub fn defpackage(ctx: *Ctx, nam: u32, use: u32) !u32 {
        const pkg = try ctx.new(.pkg, .{
            .nam = nam,
            .sym = nil,
            .use = use,
        });

        const str = try ctx.orb.dupe(u8, try ctx.v08slice(nam));

        try ctx.pkgmap.putNoClobber(ctx.orb, str, pkg);

        return pkg;
    }

    pub fn load(ctx: *Ctx, str: []const u8) !void {
        const forms = try read.readMany(ctx, str);
        defer forms.deinit();

        for (forms.items) |form| {
            var exe = eval.init(ctx, form);
            if (exe.evaluate(1_000, false)) |_| {} else |_| {
                try dump.warn("failed", ctx, form);
                try dump.warn("condition", ctx, exe.err);
                break;
            }
        }

        try tidy.tidy(ctx);
    }

    pub fn cook(ctx: *Ctx) !void {
        try ctx.load(@embedFile("./a0-base.lisp"));
        try ctx.load(@embedFile("./a1-backquote.lisp"));
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
        ctx.v32.list.deinit(ctx.orb);

        for (ctx.pkgmap.keys()) |key| {
            ctx.orb.free(key);
        }

        ctx.pkgmap.deinit(ctx.orb);

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
        if (wisp.tagOf(ptr) != tag)
            return Oof.Bug;

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
        if (wisp.tagOf(p) != tag)
            return Oof.Bug;

        return ctx.col(tag, c)[ref(p)];
    }

    pub fn set(
        ctx: *Ctx,
        comptime tag: Tag,
        comptime c: Col(tag),
        p: u32,
        v: u32,
    ) !void {
        if (wisp.tagOf(p) != tag)
            return Oof.Bug;

        ctx.col(tag, c)[ref(p)] = v;
    }

    pub fn put(
        ctx: *Ctx,
        comptime tag: Tag,
        ptr: u32,
        val: Row(tag),
    ) void {
        if (wisp.tagOf(ptr) != tag)
            return Oof.Bug;

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
        try ctx.v32.list.appendSlice(ctx.orb, dat);
        return ctx.new(.v32, .{
            .idx = @intCast(u32, ctx.v32.list.items.len - dat.len),
            .len = @intCast(u32, dat.len),
        });
    }

    pub fn v08slice(ctx: *Ctx, ptr: u32) ![]const u8 {
        const str = try ctx.row(.v08, ptr);
        return ctx.v08.items[str.idx .. str.idx + str.len];
    }

    pub fn v32slice(ctx: *Ctx, ptr: u32) ![]const u32 {
        const str = try ctx.row(.v32, ptr);
        return ctx.v32.list.items[str.idx .. str.idx + str.len];
    }

    pub fn newSymbol(ctx: *Ctx, txt: []const u8, pkg: u32) !u32 {
        return try ctx.new(.sym, .{
            .str = try ctx.newv08(txt),
            .val = nah,
            .pkg = pkg,
            .fun = nil,
        });
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

        const symptr = try ctx.newSymbol(txt, pkgptr);

        try ctx.set(.pkg, .sym, pkgptr, try ctx.new(.duo, .{
            .car = symptr,
            .cdr = pkg.sym,
        }));

        return symptr;
    }
};

pub fn list(ctx: *Ctx, xs: anytype) !u32 {
    var cur = nil;
    var i: u32 = @intCast(u32, xs.len);
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
