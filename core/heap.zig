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

const Wisp = @import("./wisp.zig");
const Tidy = @import("./tidy.zig");
const Step = @import("./step.zig");
const Sexp = @import("./sexp.zig");
const Keys = @import("./keys.zig");

const Tag = Wisp.Tag;
const Era = Wisp.Era;
const Ptr = Wisp.Ptr;

const ref = Wisp.ref;
const nil = Wisp.nil;
const nah = Wisp.nah;

/// Each pointer type has a set of columns.
pub fn ColEnum(comptime t: Tag) type {
    return switch (t) {
        .int, .sys, .chr, .jet => void,
        .duo => enum { car, cdr },
        .sym => enum { str, pkg, val, fun },
        .fun => enum { env, par, exp, sym },
        .mac => enum { env, par, exp, sym },
        .v08 => enum { idx, len },
        .v32 => enum { idx, len },
        .pkg => enum { nam, sym, use },
        .ktx => enum { hop, env, fun, acc, arg },
        .run => enum { exp, val, err, env, way },
    };
}

/// These symbols are always interned and available for easy access
/// from the Zig code.
pub const Kwd = enum {
    QUOTE,
    BACKQUOTE,
    UNQUOTE,
    @"UNQUOTE-SPLICING",

    @"&REST",

    COND,
    DEFUN,
    IF,
    FN,
    LET,
    PROGN,
    @"CALL/CC",
    EVAL,

    EXP,
    VAL,

    ERROR,
    REQUEST,

    BOOLEAN,
    CHARACTER,
    CONS,
    CONTINUATION,
    EVALUATOR,
    FUNCTION,
    INTEGER,
    MACRO,
    NULL,
    PACKAGE,
    PROMPT,
    STRING,
    SYMBOL,
    VECTOR,

    @"BAD-FIXNUM-DIVISION",
    @"BUG",
    @"CONTINUATION-CALL-ERROR",
    @"EXHAUSTED",
    @"FIXNUM-OVERFLOW",
    @"INVALID-ARGUMENT-COUNT",
    @"LOW-LEVEL-ERROR",
    @"PACKAGE-ERROR",
    @"PROGRAM-ERROR",
    @"PROMPT-TAG-MISSING",
    @"TYPE-MISMATCH",
    @"UNBOUND-VARIABLE",
    @"UNDEFINED-FUNCTION",
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
            const len = tab.list.len;

            if (len > 0 and @mod(len, 10_000) == 0) {
                const bytes = @sizeOf(Row(tag)) * len;
                std.log.info("tag {s} has {d}K rows, ~{d} KB", .{
                    @tagName(tag),
                    len / 1000,
                    bytes / 1024,
                });
            }

            try tab.list.append(orb, row);
            const idx = @intCast(u26, len);
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
    run: Tab(.run) = .{},
    ktx: Tab(.ktx) = .{},

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

pub const MsgTag = enum(u8) {
    new,
    put,
    set,
    v08,
    v32,
    era,
    pkg,
    rnd,
};

pub const Log = std.ArrayList(u8);

pub const Heap = struct {
    /// A heap can emit a mutation log for replication.
    log: ?*Log,

    orb: Orb,
    era: Era = .e0,
    vat: Vat = .{},
    v08: V08 = .{},
    v32: V32 = .{},

    kwd: S32(Kwd) = .{},

    base: u32,
    keywordPackage: u32,
    keyPackage: u32,

    pkg: u32,
    pkgmap: std.StringArrayHashMapUnmanaged(u32) = .{},

    pub fn init(orb: Orb, era: Era) !Heap {
        return initWithLog(orb, era, null);
    }

    pub fn initWithLog(orb: Orb, era: Era, log: ?*Log) !Heap {
        var heap = Heap{
            .log = log,
            .orb = orb,
            .era = era,
            .base = nil,
            .keywordPackage = nil,
            .keyPackage = nil,
            .pkg = nil,
        };

        heap.base = try heap.defpackage(
            try heap.newv08("WISP"),
            nil,
        );

        heap.keywordPackage = try heap.defpackage(
            try heap.newv08("KEYWORD"),
            nil,
        );

        heap.keyPackage = try heap.defpackage(
            try heap.newv08("KEY"),
            nil,
        );

        heap.pkg = heap.base;

        // Make symbols in WISP for the keyword cache.
        inline for (std.meta.fields(Kwd)) |s| {
            @field(heap.kwd, s.name) = try heap.intern(s.name, heap.base);
        }

        return heap;
    }

    pub fn defpackage(heap: *Heap, nam: u32, use: u32) !u32 {
        const pkg = try heap.new(.pkg, .{
            .nam = nam,
            .sym = nil,
            .use = use,
        });

        const str = try heap.orb.dupe(u8, try heap.v08slice(nam));

        try heap.pkgmap.putNoClobber(heap.orb, str, pkg);

        if (heap.log) |log| {
            try log.append(@enumToInt(MsgTag.pkg));
            const buf = try log.addManyAsArray(4);
            std.mem.writeIntLittle(u32, buf, pkg);
        }

        return pkg;
    }

    pub fn load(heap: *Heap, str: []const u8) !u32 {
        var result = nil;

        const forms = try Sexp.readMany(heap, str);
        defer forms.deinit();

        for (forms.items) |form| {
            var run = Step.initRun(form);
            result = Step.evaluate(heap, &run, 1_000_000) catch {
                try Sexp.warn("failed", heap, form);
                try Sexp.warn("condition", heap, run.err);
                break;
            };
        }

        try Tidy.gc(heap, &.{&result});

        return result;
    }

    pub fn cook(heap: *Heap) !void {
        _ = try heap.load(@embedFile("./lisp/base-0-prelude.lisp"));
        _ = try heap.load(@embedFile("./lisp/base-1-backquote.lisp"));
        _ = try heap.load(@embedFile("./lisp/base-2-stdlib.lisp"));
        _ = try heap.load(@embedFile("./lisp/base-3-repl.lisp"));
        _ = try heap.load(@embedFile("./lisp/base-x-test.lisp"));
    }

    fn initvar(heap: *Heap, txt: []const u8, val: u32) !void {
        var sym = try heap.intern(txt, heap.base);
        try heap.set(.sym, .val, sym, val);
    }

    pub fn deinit(heap: *Heap) void {
        heap.v08.deinit(heap.orb);
        heap.v32.list.deinit(heap.orb);

        for (heap.pkgmap.keys()) |key| {
            heap.orb.free(key);
        }

        heap.pkgmap.deinit(heap.orb);

        inline for (std.meta.fields(Vat)) |field| {
            @field(heap.vat, field.name).list.deinit(heap.orb);
        }
    }

    pub fn bytesize(heap: Heap) usize {
        var bytes = heap.v08.items.len;
        bytes += heap.v32.list.items.len * 4;
        bytes += heap.vat.bytesize();
        return bytes;
    }

    pub fn tab(heap: *Heap, comptime tag: Tag) *Tab(tag) {
        return &@field(heap.vat, @tagName(tag));
    }

    pub fn new(heap: *Heap, comptime tag: Tag, data: Row(tag)) !u32 {
        if (heap.log) |log| {
            const out = log.writer();
            try out.writeByte(@enumToInt(MsgTag.new));
            try out.writeByte(@typeInfo(Row(tag)).Struct.fields.len);
            inline for (std.meta.fields(Row(tag))) |f| {
                try out.writeIntLittle(u32, @field(data, f.name));
            }
        }

        return heap.tab(tag).new(heap.orb, heap.era, data);
    }

    pub fn copy(heap: *Heap, comptime tag: Tag, ptr: u32) !u32 {
        return heap.new(tag, try heap.row(tag, ptr));
    }

    pub fn cons(heap: *Heap, car: u32, cdr: u32) !u32 {
        return heap.new(.duo, .{ .car = car, .cdr = cdr });
    }

    pub fn copyAny(heap: *Heap, x: u32) !u32 {
        return switch (Wisp.tagOf(x)) {
            .int, .chr, .sys, .jet => x,
            .sym => heap.copy(.sym, x),
            .duo => heap.copy(.duo, x),
            .fun => heap.copy(.fun, x),
            .mac => heap.copy(.mac, x),
            .v32 => heap.copy(.v32, x),
            .v08 => heap.copy(.v08, x),
            .pkg => heap.copy(.pkg, x),
            .run => heap.copy(.run, x),
            .ktx => heap.copy(.ktx, x),
        };
    }

    pub fn row(heap: *Heap, comptime tag: Tag, ptr: u32) !Row(tag) {
        if (Wisp.tagOf(ptr) != tag)
            return Oof.Bug;

        return heap.tab(tag).get(heap.era, ptr);
    }

    pub fn col(
        heap: *Heap,
        comptime tag: Tag,
        comptime c: Col(tag),
    ) []u32 {
        return heap.tab(tag).col(c);
    }

    pub fn get(
        heap: *Heap,
        comptime tag: Tag,
        comptime c: Col(tag),
        p: u32,
    ) !u32 {
        if (Wisp.tagOf(p) != tag)
            return Oof.Bug;

        return heap.col(tag, c)[ref(p)];
    }

    pub fn set(
        heap: *Heap,
        comptime tag: Tag,
        comptime c: Col(tag),
        p: u32,
        v: u32,
    ) !void {
        if (Wisp.tagOf(p) != tag)
            return Oof.Bug;

        if (heap.log) |log| {
            const out = log.writer();
            try out.writeAll(&[_]u8{
                @enumToInt(MsgTag.set),
                @enumToInt(tag),
                @enumToInt(c),
            });
            try out.writeIntLittle(u32, p);
            try out.writeIntLittle(u32, v);
        }

        heap.col(tag, c)[ref(p)] = v;
    }

    pub fn put(
        heap: *Heap,
        comptime tag: Tag,
        ptr: u32,
        val: Row(tag),
    ) !void {
        if (Wisp.tagOf(ptr) != tag)
            return Oof.Bug;

        if (heap.log) |log| {
            const out = log.writer();
            const len = @typeInfo(Row(tag)).Struct.fields.len;
            try out.writeByte(@enumToInt(MsgTag.put));
            try out.writeByte(len);
            try out.writeIntLittle(u32, ptr);
            inline for (std.meta.fields(Row(tag))) |f| {
                try out.writeIntLittle(u32, @field(val, f.name));
            }
        }

        heap.tab(tag).list.set(ref(ptr), val);
    }

    pub fn newv08(heap: *Heap, dat: []const u8) !u32 {
        if (heap.log) |log| {
            const out = log.writer();
            try out.writeByte(@enumToInt(MsgTag.v08));
            try out.writeIntLittle(u32, @intCast(u32, dat.len));
            try out.writeAll(dat);
        }

        try heap.v08.appendSlice(heap.orb, dat);
        return heap.new(.v08, .{
            .idx = @intCast(u32, heap.v08.items.len - dat.len),
            .len = @intCast(u32, dat.len),
        });
    }

    pub fn newv32(heap: *Heap, dat: []const u32) !u32 {
        if (heap.log) |log| {
            const writer = log.writer();
            try writer.writeByte(@enumToInt(MsgTag.v32));
            try writer.writeIntLittle(u32, @intCast(u32, dat.len));
            for (dat) |x| {
                try writer.writeIntLittle(u32, x);
            }
        }

        try heap.v32.list.appendSlice(heap.orb, dat);
        return heap.new(.v32, .{
            .idx = @intCast(u32, heap.v32.list.items.len - dat.len),
            .len = @intCast(u32, dat.len),
        });
    }

    pub fn v08slice(heap: *Heap, ptr: u32) ![]const u8 {
        const str = try heap.row(.v08, ptr);
        return heap.v08.items[str.idx .. str.idx + str.len];
    }

    pub fn v32slice(heap: *Heap, ptr: u32) ![]u32 {
        const str = try heap.row(.v32, ptr);
        return heap.v32.list.items[str.idx .. str.idx + str.len];
    }

    pub fn newSymbol(heap: *Heap, txt: []const u8, pkg: u32) !u32 {
        return try heap.new(.sym, .{
            .str = try heap.newv08(txt),
            .val = nah,
            .pkg = pkg,
            .fun = nil,
        });
    }

    pub fn intern(heap: *Heap, txt: []const u8, pkgptr: u32) !u32 {
        if (pkgptr == heap.base) {
            if (std.mem.eql(u8, txt, "NIL")) {
                return nil;
            } else if (std.mem.eql(u8, txt, "T")) {
                return Wisp.t;
            }
        }

        const symstrs = heap.vat.sym.list.items(.str);
        const pkg = try heap.row(.pkg, pkgptr);
        var duoptr = pkg.sym;

        while (duoptr != nil) {
            const duo = try heap.row(.duo, duoptr);
            const strptr = symstrs[Ptr.from(duo.car).idx];
            if (std.mem.eql(u8, txt, try heap.v08slice(strptr))) {
                return duo.car;
            } else {
                duoptr = duo.cdr;
            }
        }

        const symptr = try heap.newSymbol(txt, pkgptr);

        try heap.set(.pkg, .sym, pkgptr, try heap.cons(symptr, pkg.sym));

        return symptr;
    }

    pub fn genkey(heap: *Heap) !u32 {
        if (heap.log) |log| {
            try log.append(@enumToInt(MsgTag.rnd));
        }

        const key = Keys.generate(&std.crypto.random);
        const sym = try heap.intern(
            &key.toZB32(),
            heap.keyPackage,
        );

        try heap.set(.sym, .val, sym, sym);
        return sym;
    }

    pub fn symstrslice(heap: *Heap, sym: u32) ![]const u8 {
        var str = try heap.get(.sym, .str, sym);
        return try heap.v08slice(str);
    }
};

pub fn list(heap: *Heap, xs: anytype) !u32 {
    var cur = nil;
    var i: u32 = @intCast(u32, xs.len);
    while (i >= 1) : (i -= 1) {
        const x = xs[i - 1];
        cur = try heap.cons(x, cur);
    }
    return cur;
}

pub fn length(heap: *Heap, x: u32) !u32 {
    var cur = x;
    var i: u32 = 0;
    while (cur != nil) : (i += 1) {
        cur = try heap.get(.duo, .cdr, cur);
    }

    return i;
}

test "heap" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();
}

test "list length" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    try same(
        @as(u32, 3),
        try length(&heap, try list(&heap, [_]u32{ 1, 2, 3 })),
    );
}

test "heap init with log" {
    var log = std.ArrayList(u8).init(std.testing.allocator);
    defer log.deinit();

    var heap = try Heap.initWithLog(std.testing.allocator, .e0, &log);
    defer heap.deinit();

    const loglen1 = log.items.len;
    try std.testing.expect(loglen1 > 1000);

    _ = try heap.new(.sym, .{ .str = 1, .val = 2, .pkg = 3, .fun = 4 });

    const loglen2 = log.items.len;
    try std.testing.expectEqual(
        @as(usize, 2 + 4 * 4),
        loglen2 - loglen1,
    );
}
