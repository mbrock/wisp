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
const Peek = @import("./peek.zig");

const Wisp = @import("./wisp.zig");
const Keys = @import("./keys.zig");
const Heap = Wisp.Heap;

const Error = error{
    ReadError,
    NoSuchPackage,
    ColonInSymbolName,
    EOF,
};

pub fn Utf8Reader(comptime ReaderType: type) type {
    return struct {
        const This = @This();

        stream: Peek.PeekStream(.{ .Static = 5 }, ReaderType),

        pub fn init(subreader: ReaderType) This {
            const stream = Peek.peekStream(5, subreader);
            return .{ .stream = stream };
        }

        pub fn peek(this: *This) !?u21 {
            var bytes: [4]u8 = undefined;
            var reader = this.stream.reader();

            if (0 == try reader.read(bytes[0..1])) {
                // EOF, no more codepoints in the stream.
                return null;
            } else {
                // The first byte tells us the codepoint length.
                const len = try std.unicode.utf8ByteSequenceLength(bytes[0]);

                // Read the full codepoint; EOF mid-codepoint is an error.
                if (len > 1) try reader.readNoEof(bytes[1..len]);

                // Put the whole codepoint back on the peek stream.
                try this.stream.putBack(bytes[0..len]);

                // Decode the UTF-8 sequence.
                return try std.unicode.utf8Decode(bytes[0..len]);
            }
        }

        pub fn read(this: *This) !?u21 {
            const c = (try this.peek()) orelse return null;
            const len = try std.unicode.utf8CodepointSequenceLength(c);

            var buf: [4]u8 = undefined;
            _ = try this.stream.read(buf[0..len]);
            return c;
        }
    };
}

pub fn Reader(comptime ReaderType: type) type {
    return struct {
        utf8: ReaderType,
        heap: *Heap,
        allocator: std.mem.Allocator,

        pub fn readValueOrEOF(self: *@This()) anyerror!?u32 {
            try self.skipSpace();

            const next = try self.peek();
            if (next) |c| {
                return switch (try classifyInitial(c)) {
                    .leftParen => try self.readList(),
                    .leftBracket => try self.readVector(),
                    .hash => try self.readHash(),
                    .colon => try self.readKeyword(),
                    .doubleQuote => try self.readString(),
                    .singleQuote => try self.readQuote(),
                    .backQuote => try self.readBackquote(),
                    .comma => try self.readUnquote(),
                    .symbolChar => try self.readSymbol(self.heap.pkg),
                    .digitChar => try self.readNumber(),
                    .tilde => try self.readKey(),
                };
            } else {
                return null;
            }
        }

        fn readValue(self: *@This()) anyerror!u32 {
            if (try self.readValueOrEOF()) |x| {
                return x;
            } else return Error.EOF;
        }

        const InitialCharType = enum {
            leftParen,
            leftBracket,
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
            } else if (c == '[') {
                return .leftBracket;
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
            self: *@This(),
            predicate: fn (u21) bool,
        ) !std.ArrayList(u8) {
            var list = std.ArrayList(u8).init(self.allocator);
            var buf: [4]u8 = undefined;

            while (try self.peek()) |c| {
                if (predicate(c)) {
                    const len = try std.unicode.utf8Encode(c, &buf);
                    try list.appendSlice(buf[0..len]);
                    _ = try self.skip();
                } else {
                    break;
                }
            }

            return list;
        }

        fn readString(
            self: *@This(),
        ) !u32 {
            try self.skipOnly('"');

            var list = std.ArrayList(u8).init(self.allocator);
            defer list.deinit();

            var buf: [4]u8 = undefined;

            while (true) {
                const c = try self.skip();
                if (c == '"') {
                    break;
                } else if (c == '\\') {
                    switch (try self.skip()) {
                        'n' => try list.appendSlice("\n"),
                        '"' => try list.appendSlice("\""),
                        '\\' => try list.appendSlice("\\"),
                        else => return error.BadEscapeChar,
                    }
                } else {
                    const len = try std.unicode.utf8Encode(c, &buf);
                    try list.appendSlice(buf[0..len]);
                }
            }

            return try self.heap.newv08(list.items);
        }

        fn readQuote(self: *@This()) !u32 {
            try self.skipOnly('\'');
            const x = try self.readValue();
            return try self.heap.cons(
                self.heap.kwd.QUOTE,
                try self.heap.cons(x, Wisp.nil),
            );
        }

        fn readBackquote(self: *@This()) !u32 {
            try self.skipOnly('`');
            const x = try self.readValue();
            return try self.heap.cons(
                self.heap.kwd.BACKQUOTE,
                try self.heap.cons(x, Wisp.nil),
            );
        }

        fn readUnquote(self: *@This()) !u32 {
            try self.skipOnly(',');

            const kwd = switch ((try self.peek()).?) {
                '@' => blk: {
                    try self.skipOnly('@');
                    break :blk self.heap.kwd.@"UNQUOTE-SPLICING";
                },
                else => self.heap.kwd.UNQUOTE,
            };

            const x = try self.readValue();
            return try self.heap.cons(
                kwd,
                try self.heap.cons(x, Wisp.nil),
            );
        }

        fn readFunctionQuote(self: *@This()) !u32 {
            try self.skipOnly('\'');
            const x = try self.readValue();
            return try self.heap.cons(
                self.heap.kwd.FUNCTION,
                try self.heap.cons(x, Wisp.nil),
            );
        }

        fn readHash(self: *@This()) !u32 {
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

        fn readChar(self: *@This()) !u32 {
            try self.skipOnly('\\');
            const c = try self.skip();
            return Wisp.Imm.make(.chr, c).word();
        }

        fn readUninternedSymbol(self: *@This()) !u32 {
            try self.skipOnly(':');

            const text = try self.readWhile(isSymbolCharacterOrDigit);
            defer text.deinit();

            const uppercase = try ziglyph.toUpperStr(
                self.heap.orb,
                text.items,
            );

            defer self.heap.orb.free(uppercase);

            return self.heap.newSymbol(uppercase, Wisp.nil);
        }

        fn readKeyword(self: *@This()) !u32 {
            try self.skipOnly(':');

            const text = try self.readWhile(isSymbolCharacterOrDigit);
            defer text.deinit();

            const uppercase = try ziglyph.toUpperStr(
                self.heap.orb,
                text.items,
            );

            defer self.heap.orb.free(uppercase);

            const sym = try self.heap.intern(
                uppercase,
                self.heap.keywordPackage,
            );

            try self.heap.set(.sym, .val, sym, sym);

            return sym;
        }

        fn readSymbol(self: *@This(), curpkg: u32) !u32 {
            const text = try self.readWhile(isSymbolCharacterOrDigit);
            defer text.deinit();

            const uppercase = try ziglyph.toUpperStr(
                self.heap.orb,
                text.items,
            );

            defer self.heap.orb.free(uppercase);

            if (std.mem.indexOf(u8, uppercase, ":")) |colon| {
                const pkgname = uppercase[0..colon];
                const symname = uppercase[colon + 1 .. uppercase.len];

                if (std.mem.count(u8, symname, ":") > 0) {
                    return Error.ColonInSymbolName;
                } else if (self.heap.pkgmap.get(pkgname)) |pkg| {
                    return self.heap.intern(symname, pkg);
                } else {
                    return Error.NoSuchPackage;
                }
            } else {
                return try self.heap.intern(uppercase, curpkg);
            }
        }

        fn readKey(self: *@This()) !u32 {
            const str = try self.readWhile(isKeyCharacter);
            defer str.deinit();

            const key = try Keys.parse(str.items);
            const sym = try self.heap.intern(
                &key.toZB32(),
                self.heap.keyPackage,
            );

            try self.heap.set(.sym, .val, sym, sym);

            return sym;
        }

        fn readNumber(self: *@This()) !u32 {
            const str = try self.readWhile(ziglyph.isAsciiDigit);
            defer str.deinit();

            var result: i31 = 0;
            var magnitude = try std.math.powi(i31, 10, @intCast(str.items.len - 1));
            for (str.items) |c| {
                result += magnitude * (c - '0');
                if (magnitude > 1) {
                    magnitude = @divExact(magnitude, 10);
                }
            }

            return @intCast(result);
        }

        fn readVector(self: *@This()) !u32 {
            try self.skipOnly('[');
            var list = std.ArrayList(u32).init(self.allocator);
            defer list.deinit();
            while (true) {
                try self.skipSpace();
                if (try self.peek()) |c| {
                    if (c == ']') {
                        try self.skipOnly(']');
                        if (list.items.len > 0) {
                            return try self.heap.newv32(list.items);
                        } else {
                            return try self.heap.emptyv32();
                        }
                    } else {
                        try list.append(try self.readValue());
                    }
                }
            }
        }

        fn readList(self: *@This()) !u32 {
            try self.skipOnly('(');
            return self.readListTail();
        }

        fn readListTail(self: *@This()) anyerror!u32 {
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
                        return self.heap.cons(car, cdr);
                    },
                }
            } else {
                return Error.EOF;
            }
        }

        fn skipOnly(self: *@This(), c: u21) !void {
            if ((try self.peek()) != c) {
                return Error.ReadError;
            }

            _ = try self.skip();
        }

        fn peek(self: *@This()) !?u21 {
            return self.utf8.peek();
        }

        fn skip(self: *@This()) !u21 {
            return (try self.utf8.read()).?;
        }

        fn skipLine(self: *@This()) !void {
            while ((try self.skip()) != '\n') {}
        }

        fn skipSpace(self: *@This()) !void {
            while (try self.peek()) |c| {
                switch (c) {
                    ' ',
                    '\n',
                    12, // ^L, page break character
                    => {
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
                else => c < 256 and Keys.zb32AlphabetMap[@intCast(c)] >= 0,
            };
        }

        fn isSymbolCharacter(c: u21) bool {
            if (ziglyph.isLetter(c)) return true;
            if (c == '`') return false;
            if (ziglyph.isSymbol(c)) return true;
            if (ziglyph.emoji.isEmojiPresentation(c)) return true;
            return switch (c) {
                '/', '_', '-', '!', '%', '&', '*', '?', '@' => true,
                else => false,
            };
        }

        fn isSymbolCharacterOrDigit(c: u21) bool {
            return isSymbolCharacter(c) or ziglyph.isAsciiDigit(c) or c == ':';
        }
    };
}

const StringStreamReader = struct {
    heap: *Heap,
    stringStream: u32,

    fn peek(this: @This()) !?u21 {
        const v32 = try this.heap.v32slice(this.stringStream);
        const i = v32[1];
        const v08 = try this.heap.v08slice(v32[2]);

        if (i < v08.len) {
            return v08[i];
        } else {
            return null;
        }
    }

    fn read(this: @This()) !?u21 {
        var v32 = try this.heap.v32slice(this.stringStream);
        const i = v32[1];
        const v08 = try this.heap.v08slice(v32[2]);

        if (i < v08.len) {
            v32[1] += 1;
            return v08[i];
        } else {
            return null;
        }
    }
};

pub fn makeStringStreamReader(
    heap: *Heap,
    tmp: std.mem.Allocator,
    stringStream: u32,
) Reader(StringStreamReader) {
    return Reader(StringStreamReader){
        .utf8 = StringStreamReader{
            .heap = heap,
            .stringStream = stringStream,
        },
        .heap = heap,
        .allocator = tmp,
    };
}

pub fn makeReader(
    heap: *Heap,
    tmp: std.mem.Allocator,
    reader: anytype,
) Reader(Utf8Reader(@TypeOf(reader))) {
    return Reader(Utf8Reader(@TypeOf(reader))){
        .utf8 = Utf8Reader(@TypeOf(reader)).init(reader),
        .heap = heap,
        .allocator = tmp,
    };
}

pub fn readValueFromStream(heap: *Heap, stream: anytype) !?u32 {
    var tmp = std.heap.stackFallback(512, heap.orb);
    var reader = makeReader(heap, tmp.get(), stream);
    return reader.readValueOrEOF();
}

pub fn readFromStringStream(heap: *Heap, stream: u32) !?u32 {
    var tmp = std.heap.stackFallback(512, heap.orb);
    var reader = makeStringStreamReader(heap, tmp.get(), stream);
    return reader.readValueOrEOF();
}

pub fn read(heap: *Heap, text: []const u8) !u32 {
    var tmp = std.heap.stackFallback(512, heap.orb);
    var stream = std.io.fixedBufferStream(text);
    var reader = makeReader(heap, tmp.get(), stream.reader());
    return reader.readValue();
}

pub fn readMany(heap: *Heap, text: []const u8) !std.ArrayList(u32) {
    var tmp = std.heap.stackFallback(512, heap.orb);
    var list = std.ArrayList(u32).init(heap.orb);
    var stream = std.io.fixedBufferStream(text);
    var reader = makeReader(heap, tmp.get(), stream.reader());

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
