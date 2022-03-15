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
const ziglyph = @import("ziglyph");

const Wisp = @import("./wisp.zig");
const Keys = @import("./keys.zig");
const Heap = Wisp.Heap;

const Error = error{
    ReadError,
    EOF,
};

const Reader = @This();

utf8: std.unicode.Utf8Iterator,
heap: *Heap,

fn readValueOrEOF(self: *Reader) anyerror!?u32 {
    try self.skipSpace();

    const next = try self.peek();
    if (next) |c| {
        return switch (try classifyInitial(c)) {
            .leftParen => try self.readList(),
            .hash => try self.readHash(),
            .colon => try self.readKeyword(),
            .doubleQuote => try self.readString(),
            .singleQuote => try self.readQuote(),
            .backQuote => try self.readQuasiquote(),
            .comma => try self.readUnquote(),
            .symbolChar => try self.readSymbol(self.heap.pkg),
            .digitChar => try self.readNumber(),
            .tilde => try self.readKey(),
        };
    } else {
        return null;
    }
}

fn readValue(self: *Reader) anyerror!u32 {
    return if (try self.readValueOrEOF()) |x| x else Error.EOF;
}

const InitialCharType = enum {
    leftParen,
    hash,
    colon,
    symbolChar,
    digitChar,
    doubleQuote,
    singleQuote,
    backQuote,
    comma,
    tilde,
};

fn classifyInitial(c: u21) !InitialCharType {
    if (c == '(') {
        return .leftParen;
    } else if (c == '#') {
        return .hash;
    } else if (c == ':') {
        return .colon;
    } else if (isSymbolCharacter(c)) {
        return .symbolChar;
    } else if (ziglyph.isAsciiDigit(c)) {
        return .digitChar;
    } else if (c == '"') {
        return .doubleQuote;
    } else if (c == '\'') {
        return .singleQuote;
    } else if (c == '`') {
        return .backQuote;
    } else if (c == ',') {
        return .comma;
    } else if (c == '~') {
        return .tilde;
    } else {
        var out: [32]u8 = undefined;
        const len = try std.unicode.utf8Encode(c, &out);
        std.log.err("unexpected {s}", .{out[0..len]});
        return Error.ReadError;
    }
}

fn readWhile(
    self: *Reader,
    predicate: fn (u21) bool,
) ![]const u8 {
    var scout = std.unicode.Utf8Iterator{
        .bytes = self.utf8.bytes,
        .i = self.utf8.i,
    };

    var n: usize = 0;
    while (true) {
        const slice = scout.peek(1);
        if (slice.len == 0) {
            break;
        } else {
            const c = try std.unicode.utf8Decode(slice);
            if (predicate(c)) {
                n += 1;
                _ = scout.nextCodepoint();
            } else {
                break;
            }
        }
    }

    const text = self.utf8.peek(n);
    self.utf8 = scout;

    // std.log.warn("skip {s}", .{text});

    return text;
}

fn readQuote(self: *Reader) !u32 {
    try self.skipOnly('\'');
    const x = try self.readValue();
    return try self.heap.new(.duo, .{
        .car = self.heap.kwd.QUOTE,
        .cdr = try self.heap.new(.duo, .{
            .car = x,
            .cdr = Wisp.nil,
        }),
    });
}

fn readQuasiquote(self: *Reader) !u32 {
    try self.skipOnly('`');
    const x = try self.readValue();
    return try self.heap.new(.duo, .{
        .car = self.heap.kwd.QUASIQUOTE,
        .cdr = try self.heap.new(.duo, .{
            .car = x,
            .cdr = Wisp.nil,
        }),
    });
}

fn readUnquote(self: *Reader) !u32 {
    try self.skipOnly(',');
    const x = try self.readValue();
    return try self.heap.new(.duo, .{
        .car = self.heap.kwd.UNQUOTE,
        .cdr = try self.heap.new(.duo, .{
            .car = x,
            .cdr = Wisp.nil,
        }),
    });
}

fn readFunctionQuote(self: *Reader) !u32 {
    try self.skipOnly('\'');
    const x = try self.readValue();
    return try self.heap.new(.duo, .{
        .car = self.heap.kwd.FUNCTION,
        .cdr = try self.heap.new(.duo, .{
            .car = x,
            .cdr = Wisp.nil,
        }),
    });
}

fn readHash(self: *Reader) !u32 {
    try self.skipOnly('#');
    return switch ((try self.peek()).?) {
        ':' => self.readUninternedSymbol(),
        '\\' => self.readChar(),
        '\'' => self.readFunctionQuote(),
        else => |c| {
            var out: [32]u8 = undefined;
            const len = try std.unicode.utf8Encode(c, &out);
            std.log.err("skip {s}", .{out[0..len]});
            unreachable;
        },
    };
}

fn readChar(self: *Reader) !u32 {
    try self.skipOnly('\\');
    const c = try self.skip();
    return Wisp.Imm.make(.chr, c).word();
}

fn readUninternedSymbol(self: *Reader) !u32 {
    try self.skipOnly(':');

    const text = try self.readWhile(isSymbolCharacterOrDigit);
    const uppercase = try ziglyph.toUpperStr(
        self.heap.orb,
        text,
    );

    defer self.heap.orb.free(uppercase);

    return self.heap.newSymbol(uppercase, Wisp.nil);
}

fn readKeyword(self: *Reader) !u32 {
    try self.skipOnly(':');

    const text = try self.readWhile(isSymbolCharacterOrDigit);
    const uppercase = try ziglyph.toUpperStr(
        self.heap.orb,
        text,
    );

    defer self.heap.orb.free(uppercase);

    const sym = try self.heap.intern(
        uppercase,
        self.heap.keywordPackage,
    );

    try self.heap.set(.sym, .val, sym, sym);

    return sym;
}

fn readSymbol(self: *Reader, pkg: u32) !u32 {
    const text = try self.readWhile(isSymbolCharacterOrDigit);
    const uppercase = try ziglyph.toUpperStr(
        self.heap.orb,
        text,
    );

    defer self.heap.orb.free(uppercase);

    return try self.heap.intern(uppercase, pkg);
}

fn readKey(self: *Reader) !u32 {
    const str = try self.readWhile(isKeyCharacter);
    const key = try Keys.parse(str);
    const sym = try self.heap.intern(
        &key.toZB32(),
        self.heap.keyPackage,
    );

    // var buf: [8]u8 = undefined;
    // std.mem.writeIntSlice(u16, buf[0..2], key.day, .Little);
    // std.mem.writeIntSlice(u48, buf[2..8], key.rnd, .Little);

    // const v08 = try self.heap.newv08(&buf);

    try self.heap.set(.sym, .val, sym, sym);

    return sym;
}

fn readNumber(self: *Reader) !u32 {
    const str = try self.readWhile(ziglyph.isAsciiDigit);

    var result: i31 = 0;
    var magnitude = std.math.pow(i31, 10, @intCast(i31, str.len - 1));
    for (str) |c| {
        result += magnitude * (c - '0');
        if (magnitude > 1) {
            magnitude = @divExact(magnitude, 10);
        }
    }

    return @intCast(u32, result);
}

fn readString(self: *Reader) !u32 {
    try self.skipOnly('"');
    const text = try self.readWhile(isNotEndOfString);
    try self.skipOnly('"');
    return try self.heap.newv08(text);
}

fn readList(self: *Reader) !u32 {
    try self.skipOnly('(');
    return self.readListTail();
}

fn readListTail(self: *Reader) anyerror!u32 {
    try self.skipSpace();
    const next = try self.peek();
    if (next) |c| {
        switch (c) {
            ')' => {
                try self.skipOnly(')');
                return Wisp.nil;
            },

            '.' => {
                try self.skipOnly('.');
                const cdr = try self.readValue();
                try self.skipSpace();
                try self.skipOnly(')');
                return cdr;
            },

            else => {
                const car = try self.readValue();
                const cdr = try self.readListTail();
                return self.heap.new(.duo, .{
                    .car = car,
                    .cdr = cdr,
                });
            },
        }
    } else {
        return Error.EOF;
    }
}

fn skipOnly(self: *Reader, c: u21) !void {
    if ((try self.peek()) != c) {
        return Error.ReadError;
    }

    _ = try self.skip();
}

fn peek(self: *Reader) !?u21 {
    const slice = self.utf8.peek(1);
    if (slice.len == 0) {
        return null;
    } else {
        return try std.unicode.utf8Decode(slice);
    }
}

fn skip(self: *Reader) !u21 {
    const c = self.utf8.nextCodepoint().?;
    // var out: [32]u8 = undefined;
    // const len = try std.unicode.utf8Encode(c, &out);
    // std.log.err("skip {s}", .{out[0..len]});
    return c;
}

fn skipLine(self: *Reader) !void {
    while ((try self.skip()) != '\n') {}
}

fn skipSpace(self: *Reader) !void {
    while (try self.peek()) |c| {
        switch (c) {
            ' ', '\n' => {
                _ = try self.skip();
            },

            ';' => {
                try self.skipLine();
            },

            else => {
                return;
            },
        }
    }
}

fn isNotEndOfString(c: u21) bool {
    return c != '"';
}

fn isKeyCharacter(c: u21) bool {
    if (ziglyph.isAsciiDigit(c))
        return true;

    return switch (c) {
        '~', '.' => true,
        else => c < 256 and Keys.zb32AlphabetMap[@intCast(u8, c)] >= 0,
    };
}

fn isSymbolCharacter(c: u21) bool {
    if (ziglyph.isLetter(c)) {
        return true;
    } else {
        return switch (c) {
            '$',
            '%',
            '&',
            '*',
            '+',
            '-',
            '/',
            '<',
            '=',
            '>',
            '?',
            '@',
            '^',
            => true,
            else => false,
        };
    }
}

fn isSymbolCharacterOrDigit(c: u21) bool {
    return isSymbolCharacter(c) or ziglyph.isAsciiDigit(c);
}

pub fn read(heap: *Heap, stream: []const u8) !u32 {
    var reader = Reader{
        .utf8 = (try std.unicode.Utf8View.init(stream)).iterator(),
        .heap = heap,
    };

    return reader.readValue();
}

pub fn readMany(heap: *Heap, stream: []const u8) !std.ArrayList(u32) {
    var list = std.ArrayList(u32).init(heap.orb);

    var reader = Reader{
        .utf8 = (try std.unicode.Utf8View.init(stream)).iterator(),
        .heap = heap,
    };

    while (true) {
        if (try reader.readValueOrEOF()) |x| {
            try list.append(x);
        } else {
            return list;
        }
    }
}

test "read symbol uppercasing" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();

    const symbol = try read(&heap, "foobar");
    const row = try heap.row(.sym, symbol);
    const name = try heap.v08slice(row.str);

    try std.testing.expectEqualStrings(
        "FOOBAR",
        name,
    );
}

test "read nil" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();
    try std.testing.expectEqual(Wisp.nil, try read(&heap, "nil"));
}

test "read key" {
    var heap = try Heap.init(std.testing.allocator, .e0);
    defer heap.deinit();
    _ = try read(&heap, "~20220314.8NJAFJ7WJF");
}
