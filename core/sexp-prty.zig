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
const Gpa = std.mem.Allocator;

const Wisp = @import("./wisp.zig");
const Sexp = @import("./sexp.zig");
const Step = @import("./step.zig");

const Heap = Wisp.Heap;

const This = @This();

const Str = std.ArrayListUnmanaged(u8);
const Txt = std.ArrayListUnmanaged(Str);

fn copy(gpa: Gpa, str: Str) !Str {
    var new = try Str.initCapacity(gpa, str.items.len);
    try new.appendSlice(gpa, str.items);
    return new;
}

fn insertIndent(n: u16, str: *Str, gpa: Gpa) !void {
    var i: u16 = 0;
    while (i < n) : (i += 1)
        try str.insert(gpa, 0, ' ');
}

const Box = struct {
    txt: Txt = .{},
    len: u16,
    fin: u16,
    max: u16,

    pub fn text(bytes: []const u8, gpa: Gpa) !Box {
        var str = try Str.initCapacity(gpa, bytes.len);
        var txt = try Txt.initCapacity(gpa, 1);

        try str.appendSlice(gpa, bytes);
        try txt.append(gpa, str);

        return Box{
            .len = 0,
            .fin = @intCast(u16, str.items.len),
            .max = @intCast(u16, str.items.len),
            .txt = txt,
        };
    }

    pub fn deinit(box: *Box, gpa: Gpa) void {
        for (box.txt.items) |*str| str.deinit(gpa);
        box.txt.deinit(gpa);
    }

    pub fn indent(box: Box, n: u16, gpa: Gpa) !Box {
        var box2 = Box{
            .len = box.len,
            .fin = box.fin + n,
            .max = box.max + n,
        };

        for (box.txt.items) |str| {
            var str2 = try copy(gpa, str);
            try insertIndent(n, &str2, gpa);
            try box2.txt.append(gpa, str2);
        }

        return box2;
    }

    pub fn flush(a: Box, gpa: Gpa) !Box {
        var b = Box{
            .len = a.len + 1,
            .max = a.max,
            .fin = 0,
            .txt = try Txt.initCapacity(gpa, a.len + 2),
        };

        try b.txt.appendSlice(gpa, a.txt.items);
        try b.txt.append(gpa, Str{});

        return b;
    }

    pub fn hcat(a: Box, b: Box, gpa: Gpa) !Box {
        var c = Box{
            .len = a.len + b.len,
            .fin = a.fin + b.fin,
            .max = std.math.max(a.max, b.max + a.fin),
            .txt = try Txt.initCapacity(gpa, a.len + b.len),
        };

        for (a.txt.items[0..a.len]) |as| {
            var str = Str{};
            try str.appendSlice(gpa, as.items);
            try c.txt.append(gpa, str);
        }

        const lastA = a.txt.items[a.len];
        const firstB = b.txt.items[0];

        var ab = try copy(gpa, lastA);
        try ab.appendSlice(gpa, firstB.items);

        try c.txt.append(gpa, ab);

        for (b.txt.items[1 .. b.len + 1]) |bs| {
            var x = try copy(gpa, bs);
            try insertIndent(@intCast(u16, lastA.items.len), &x, gpa);
            try c.txt.append(gpa, x);
        }

        return c;
    }

    pub fn vcat(a: Box, b: Box, gpa: Gpa) !Box {
        var flushed = try a.flush(gpa);
        return try flushed.hcat(b, gpa);
    }

    pub fn render(box: Box, gpa: Gpa) ![]const u8 {
        var result = Str{};
        for (box.txt.items) |str, i| {
            if (i > 0) try result.append(gpa, '\n');
            try result.appendSlice(gpa, str.items);
        }

        return result.toOwnedSlice(gpa);
    }

    pub fn beats(a: Box, b: Box) bool {
        return a.len <= b.len and a.max <= b.max and a.fin <= b.fin;
    }
};

pub fn pareto(boxes: []const Box, gpa: Gpa) ![]const Box {
    var acc = std.ArrayList(Box).init(gpa);

    boxloop: for (boxes) |x| {
        for (acc.items) |a| {
            if (a.beats(x)) {
                continue :boxloop;
            }
        }

        var i: usize = 0;
        while (i < acc.items.len) {
            if (x.beats(acc.items[i])) {
                _ = acc.orderedRemove(i);
            } else {
                i += 1;
            }
        }

        try acc.append(x);
    }

    return acc.toOwnedSlice();
}

const Boxes = std.ArrayListUnmanaged(Box);

pub fn single(comptime T: type, gpa: Gpa, x: T) ![]T {
    var one = try gpa.alloc(T, 1);
    one[0] = x;
    return one;
}

const Doc = []const Box;

pub fn choose(gpa: Gpa, a: Doc, b: Doc) !Doc {
    var xs = try Boxes.initCapacity(
        gpa,
        a.len + b.len,
    );

    try xs.appendSlice(gpa, a);
    try xs.appendSlice(gpa, b);

    return try pareto(xs.items, gpa);
}

pub fn hcat(gpa: Gpa, xs: Doc, ys: Doc) !Doc {
    var zs = Boxes{};
    for (xs) |x| {
        for (ys) |y| {
            const xy = try x.hcat(y, gpa);
            if (xy.max < 78) {
                try zs.append(gpa, xy);
            }
        }
    }

    return try pareto(zs.items, gpa);
}

pub fn flush(gpa: Gpa, xs: Doc) !Doc {
    var ys = Boxes{};
    for (xs) |x| {
        try ys.append(gpa, try x.flush(gpa));
    }

    return ys.toOwnedSlice(gpa);
}

pub fn vcat(gpa: Gpa, xs: Doc, ys: Doc) !Doc {
    return hcat(gpa, try flush(gpa, xs), ys);
}

pub fn text(gpa: Gpa, s: []const u8) !Doc {
    return single(Box, gpa, try Box.text(s, gpa));
}

pub fn cat(gpa: Gpa, xs: Doc, ys: Doc) !Doc {
    return choose(gpa, try hcat(gpa, xs, ys), try vcat(gpa, xs, ys));
}

fn shorter(x: void, a: Box, b: Box) bool {
    _ = x;
    return a.len < b.len;
}

pub fn render(gpa: Gpa, xs: Doc) !?[]const u8 {
    if (std.sort.min(Box, xs, {}, shorter)) |best| {
        return try best.render(gpa);
    } else {
        return null;
    }
}

pub fn hspace(gpa: Gpa, a: Doc, b: Doc) !Doc {
    return hcat(gpa, a, try hcat(gpa, try text(gpa, " "), b));
}

pub fn hsep(gpa: Gpa, xss: []const Doc) !Doc {
    var ys = xss[0];
    for (xss[1..xss.len]) |xs| {
        ys = try hspace(gpa, ys, xs);
    }
    return ys;
}

pub fn hjoin(gpa: Gpa, xss: []const Doc) !Doc {
    var ys = xss[0];
    for (xss[1..xss.len]) |xs| {
        ys = try hcat(gpa, ys, xs);
    }
    return ys;
}

pub fn vjoin(gpa: Gpa, xss: []const Doc) !Doc {
    var ys = xss[0];
    for (xss[1..xss.len]) |xs| {
        ys = try vcat(gpa, ys, xs);
    }
    return ys;
}

pub fn shove(gpa: Gpa, xs: Doc) !Doc {
    var ys = std.ArrayListUnmanaged(Box){};
    for (xs) |x| {
        try ys.append(gpa, try x.indent(2, gpa));
    }

    return ys.toOwnedSlice(gpa);
}

pub fn join(gpa: Gpa, xss: []const Doc) !Doc {
    if (xss.len == 0) {
        return text(gpa, "");
    } else {
        return choose(
            gpa,
            try hsep(gpa, xss),
            try vjoin(gpa, xss),
        );
    }
}

fn hangList(heap: *Wisp.Heap, items: []u32) ?usize {
    if (hang(heap, items[0])) |n| {
        if (n < items.len)
            return n;
    }

    return null;
}

fn hang(heap: *Wisp.Heap, car: u32) ?usize {
    if (car == heap.kwd.DEFUN)
        return 3
    else if (car == heap.kwd.IF)
        return 2
    else if (car == heap.kwd.LET)
        return 2
    else if (car == heap.kwd.FN)
        return 2
    else if (car == heap.kwd.COND)
        return 1
    else if (car == heap.kwd.DO)
        return 1
    else if (car == heap.kwd.@"CALL/CC")
        return 1
    else
        return null;
}

pub fn pretty(gpa: Gpa, heap: *Wisp.Heap, exp: u32) anyerror!Doc {
    var tmp = std.heap.stackFallback(32, heap.orb);

    switch (Wisp.tagOf(exp)) {
        .duo => {
            var list = try Step.scanListAllocAllowDotted(heap, tmp.get(), exp);

            var array = list.arrayList();
            defer array.deinit();

            var items = array.items;

            if (hangList(heap, array.items)) |n| {
                if (list.isDotted())
                    return Wisp.Oof.Err;

                var xs = std.ArrayListUnmanaged(Doc){};
                var ys = std.ArrayListUnmanaged(Doc){};

                for (items[0..n]) |x| {
                    try xs.append(gpa, try pretty(gpa, heap, x));
                }

                for (items[n..items.len]) |x| {
                    try ys.append(gpa, try pretty(gpa, heap, x));
                }

                return vcat(
                    gpa,
                    try hcat(
                        gpa,
                        try text(gpa, "("),
                        try join(gpa, xs.items),
                    ),
                    try shove(
                        gpa,

                        try hcat(
                            gpa,
                            try vjoin(gpa, ys.items),
                            try text(gpa, ")"),
                        ),
                    ),
                );
            } else if (items.len > 2 and Wisp.tagOf(items[0]) == .sym) {
                var cdr = std.ArrayListUnmanaged(Doc){};
                for (items[1..items.len]) |x, n| {
                    if (n == items.len - 1) {
                        try cdr.append(gpa, try text(gpa, "."));
                    }

                    try cdr.append(gpa, try pretty(gpa, heap, x));
                }

                return hjoin(gpa, &[_]Doc{
                    try text(gpa, "("),
                    try pretty(gpa, heap, items[0]),
                    try text(gpa, " "),
                    try join(gpa, cdr.items),
                    try text(gpa, ")"),
                });
            } else {
                var xs = std.ArrayListUnmanaged(Doc){};
                for (items[0..items.len]) |x| {
                    try xs.append(gpa, try pretty(gpa, heap, x));
                }
                return hjoin(gpa, &[_]Doc{
                    try text(gpa, "("),
                    try join(gpa, xs.items),
                    try text(gpa, ")"),
                });
            }
        },

        .v32 => {
            var items = try heap.v32slice(exp);
            var array = std.ArrayListUnmanaged(Doc){};
            for (items) |x| {
                try array.append(gpa, try pretty(gpa, heap, x));
            }

            return hjoin(gpa, &[_]Doc{
                try text(gpa, "["),
                try join(gpa, array.items),
                try text(gpa, "]"),
            });
        },

        .ktx => {
            const ktx = try heap.row(.ktx, exp);
            return hjoin(gpa, &.{
                try text(gpa, "<ktx "),
                try join(gpa, &.{
                    try pretty(gpa, heap, ktx.fun),
                    try pretty(gpa, heap, ktx.acc),
                    try pretty(gpa, heap, ktx.arg),
                    try pretty(gpa, heap, ktx.env),
                    try pretty(gpa, heap, ktx.hop),
                }),
                try text(gpa, ">"),
            });
        },

        else => {
            const s = try Sexp.printAlloc(gpa, heap, exp);
            return text(gpa, s);
        },
    }
}

pub fn prettyPrint(heap: *Wisp.Heap, exp: u32, max: u32) ![]const u8 {
    _ = max;

    var arena = std.heap.ArenaAllocator.init(heap.orb);
    defer arena.deinit();
    var gpa = arena.allocator();

    var doc = try pretty(gpa, heap, exp);
    var str = (try render(gpa, doc)).?;

    return heap.orb.dupe(u8, str);
}

test "box hcat" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var gpa = arena.allocator();

    var aaaaa = try Box.text("aaaaa", gpa);
    var aa = try Box.text("aa", gpa);
    var bbbbb = try Box.text("bbbbb", gpa);
    var bbbbbbb = try Box.text("bbbbbbb", gpa);

    var a = try aaaaa.vcat(aa, gpa);
    var b = try bbbbb.vcat(bbbbbbb, gpa);
    var ab = try a.hcat(b, gpa);

    try std.testing.expectEqualStrings(
        \\aaaaa
        \\aabbbbb
        \\  bbbbbbb
    ,
        try ab.render(gpa),
    );
}

test "pareto" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var gpa = arena.allocator();

    const boxes = try pareto(
        &[_]Box{
            .{
                .len = 0,
                .fin = 0,
                .max = 0,
            },
            .{
                .len = 1,
                .fin = 0,
                .max = 0,
            },
        },
        gpa,
    );

    try std.testing.expectEqualSlices(
        Box,
        &[_]Box{
            .{
                .len = 0,
                .fin = 0,
                .max = 0,
            },
        },
        boxes,
    );
}

test "prty" {
    const exampleCode =
        \\(defun box<= (a b)
        \\ (and (<= (box-len a) (box-len b))
        \\      (<= (box-max a) (box-max b))
        \\      (<= (box-fin a) (box-fin b))))
    ;

    const expected =
        \\(DEFUN BOX<= (A B)
        \\  (AND (<= (BOX-LEN A) (BOX-LEN B))
        \\       (<= (BOX-MAX A) (BOX-MAX B))
        \\       (<= (BOX-FIN A) (BOX-FIN B))))
    ;

    var heap = try Wisp.Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    const example = try Sexp.read(&heap, exampleCode);
    const actual = try prettyPrint(&heap, example, 62);

    defer heap.orb.free(actual);

    try std.testing.expectEqualStrings(expected, actual);
}
