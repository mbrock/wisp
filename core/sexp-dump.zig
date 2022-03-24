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

const Wisp = @import("./wisp.zig");
const Jets = @import("./jets.zig");
const Prty = @import("./sexp-prty.zig");

const Heap = Wisp.Heap;

test "print one" {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    try list.writer().print("{}", .{1});
    try std.testing.expectEqualStrings("1", list.items);
}

pub fn expectDump(
    expected: []const u8,
    heap: *Heap,
    x: u32,
) !void {
    var actual = try printAlloc(std.testing.allocator, heap, x);
    defer std.testing.allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);
}

pub fn printAlloc(
    allocator: std.mem.Allocator,
    heap: *Heap,
    word: u32,
) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    try dump(heap, list.writer(), word);
    return list.toOwnedSlice();
}

pub fn warn(prefix: []const u8, heap: *Heap, word: u32) !void {
    var s = try Prty.prettyPrint(heap, word, 72);
    defer heap.orb.free(s);
    const stderr = std.io.getStdErr().writer();
    try stderr.print("; {s}\n{s}\n", .{ prefix, s });
}

pub fn dump(heap: *Heap, out: anytype, x: u32) anyerror!void {
    switch (Wisp.tagOf(x)) {
        .int => try out.print("{d}", .{@bitCast(i31, @intCast(u31, x))}),

        .sys => {
            switch (x) {
                Wisp.nil => try out.print("NIL", .{}),
                Wisp.t => try out.print("T", .{}),
                Wisp.top => try out.print("#<TOP>", .{}),
                Wisp.nah => try out.print("#<NAH>", .{}),
                else => unreachable,
            }
        },

        .sym => {
            const sym = try heap.row(.sym, x);
            const pkg = sym.pkg;
            const name = heap.v08slice(sym.str);
            if (pkg == Wisp.nil) {
                try out.print("#:{s}", .{name});
            } else if (pkg == heap.keywordPackage) {
                try out.print(":{s}", .{name});
            } else if (pkg == heap.keyPackage) {
                try out.print("{s}", .{name});
            } else if (pkg == heap.pkg) {
                try out.print("{s}", .{name});
            } else {
                const pkgname = try heap.get(.pkg, .nam, pkg);
                const pkgstr = heap.v08slice(pkgname);
                try out.print("{s}:{s}", .{ pkgstr, name });
            }
        },

        .v08 => {
            const s = heap.v08slice(x);
            try out.print("\"{s}\"", .{s});
        },

        .v32 => {
            try out.print("#<", .{});
            const xs = try heap.v32slice(x);
            for (xs) |y, i| {
                if (i > 0) try out.print(" ", .{});
                try dump(heap, out, y);
            }
            try out.print(">", .{});
        },

        .duo => {
            try out.print("(", .{});
            var cur = x;

            loop: while (cur != Wisp.nil) {
                var cons = try heap.row(.duo, cur);
                try dump(heap, out, cons.car);
                switch (Wisp.tagOf(cons.cdr)) {
                    .duo => {
                        try out.print(" ", .{});
                        cur = cons.cdr;
                    },
                    else => {
                        if (cons.cdr != Wisp.nil) {
                            try out.print(" . ", .{});
                            try dump(heap, out, cons.cdr);
                        }
                        break :loop;
                    },
                }
            }

            try out.print(")", .{});
        },

        .pkg => {
            try out.print("<package>", .{});
        },

        .ktx => {
            const ktx = try heap.row(.ktx, x);
            try out.print("<%ktx", .{});
            inline for (std.meta.fields(@TypeOf(ktx))) |field| {
                try out.print(" {s}=", .{field.name});
                try dump(heap, out, @field(ktx, field.name));
            }
            try out.print(">", .{});
        },

        .fun => {
            const sym = try heap.get(.fun, .sym, x);
            if (sym == Wisp.nil)
                try out.print("#<ANONYMOUS-FUNCTION>", .{})
            else
                try out.print("#'{s}", .{try heap.symstrslice(sym)});
        },

        .mac => {
            const sym = try heap.get(.mac, .sym, x);
            if (sym == Wisp.nil)
                try out.print("#<ANONYMOUS-MACRO>", .{})
            else
                try out.print("#'{s}", .{try heap.symstrslice(sym)});
        },

        .jet => {
            const jet = Jets.jets[Wisp.Imm.from(x).idx];
            try out.print("{s}", .{jet.txt});
        },

        .run => {
            const run = try heap.row(.run, x);
            try out.print("<run", .{});
            inline for (std.meta.fields(@TypeOf(run))) |field| {
                try out.print(" {s}=", .{field.name});
                try dump(heap, out, @field(run, field.name));
            }
            try out.print(">", .{});
        },

        else => |t| try out.print("<{any}>", .{t}),
    }
}

fn expectPrintResult(heap: *Heap, expected: []const u8, x: u32) !void {
    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    try dump(heap, &writer, x);
    try std.testing.expectEqualStrings(expected, list.items);
}

test "print fixnum" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    try expectPrintResult(&heap, "1", 1);
}

test "print constants" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    try expectPrintResult(&heap, "NIL", Wisp.nil);
    try expectPrintResult(&heap, "T", Wisp.t);
}

test "print lists" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "(1 2 3)",
        try Wisp.list(&heap, [_]u32{ 1, 2, 3 }),
    );

    try expectPrintResult(&heap, "(1 . 2)", try heap.cons(1, 2));
}

test "print symbols" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "FOO",
        try heap.intern("FOO", heap.base),
    );
}

test "print uninterned symbols" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "#:FOO",
        try heap.newSymbol("FOO", Wisp.nil),
    );
}

test "print keys" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "~20220314.8NJAFJ7WJF",
        try heap.newSymbol("~20220314.8NJAFJ7WJF", heap.keyPackage),
    );
}

// test "print structs" {
//     var heap = try Heap.init(std.testing.allocator);
//     defer heap.deinit();

//     try expectPrintResult(
//         &heap,
//         "«instance PACKAGE \"WISP\"»",
//         0,
//     );
// }

test "print strings" {
    var heap = try Heap.init(std.testing.allocator, .e1);
    defer heap.deinit();

    try expectPrintResult(
        &heap,
        "\"hello\"",
        try heap.newv08("hello"),
    );
}
