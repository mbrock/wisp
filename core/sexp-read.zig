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
const Keys = @import("./keys.zig");
const Heap = Wisp.Heap;

fn ensureUnicode(_: std.mem.Allocator) !void {}

fn toUpperStr(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    const upper = try allocator.alloc(u8, str.len);
    for (str, 0..) |ch, i| upper[i] = std.ascii.toUpper(ch);
    return upper;
}

fn isAsciiDigit(c: u21) bool {
    return c >= '0' and c <= '9';
}

fn isInRanges(c: u21, ranges: []const [2]u21) bool {
    for (ranges) |range| {
        if (c >= range[0] and c <= range[1]) return true;
    }
    return false;
}

fn isLetter(c: u21) bool {
    const letterish = [_][2]u21{
        .{ 'a', 'z' },
        .{ 'A', 'Z' },
        // Latin extended blocks
        .{ 0x00C0, 0x02AF },
        // Greek and Coptic, Greek Extended
        .{ 0x0370, 0x03FF },
        .{ 0x1F00, 0x1FFF },
        // Cyrillic
        .{ 0x0400, 0x052F },
        // Armenian, Hebrew, Arabic, Syriac
        .{ 0x0530, 0x077F },
        // Devanagari and Indic scripts
        .{ 0x0900, 0x0D7F },
        // Latin Extended Additional, Vietnamese, etc.
        .{ 0x1E00, 0x1EFF },
        // Phonetic extensions
        .{ 0x1D00, 0x1DBF },
        // Unified Canadian Aboriginal Syllabics
        .{ 0x1400, 0x167F },
        // Ethiopic
        .{ 0x1200, 0x137F },
        // Cherokee, Ogham, Runic
        .{ 0x13A0, 0x13FF },
        .{ 0x16A0, 0x16FF },
        // Georgian, Hangul Jamo
        .{ 0x10A0, 0x10FF },
        .{ 0x1100, 0x11FF },
        // Hiragana, Katakana, Bopomofo, Hangul syllables
        .{ 0x3040, 0x30FF },
        .{ 0x31A0, 0x31FF },
        .{ 0xAC00, 0xD7A3 },
        // Common Ideograph ranges (treat as letters for symbol parsing purposes)
        .{ 0x4E00, 0x9FFF },
        .{ 0x3400, 0x4DBF },
    };
    return isInRanges(c, &letterish);
}

fn isSymbol(c: u21) bool {
    if (c <= 0x7F) {
        return switch (c) {
            '!', '#', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '^', '_', '|', '~', '[', ']', '{', '}', ',', ';', '`' => true,
            else => false,
        };
    }
    const symbolish = [_][2]u21{
        .{ 0x2000, 0x206F }, // General punctuation
        .{ 0x20A0, 0x20CF }, // Currency symbols
        .{ 0x2100, 0x218F }, // Letterlike symbols, number forms
        .{ 0x2190, 0x21FF }, // Arrows
        .{ 0x2200, 0x22FF }, // Mathematical operators
        .{ 0x2300, 0x23FF },
        .{ 0x2460, 0x24FF },
        .{ 0x25A0, 0x2BFF },
    };
    return isInRanges(c, &symbolish);
}

fn isEmojiPresentation(c: u21) bool {
    return isInRanges(c, &.{
        .{ 0x1F300, 0x1F5FF },
        .{ 0x1F600, 0x1F64F },
        .{ 0x1F680, 0x1F6FF },
        .{ 0x1F900, 0x1FAD0 },
        .{ 0x1FA70, 0x1FAFF },
    });
}

const Error = error{
    ReadError,
    NoSuchPackage,
    ColonInSymbolName,
    EOF,
};

pub fn Utf8Reader(comptime ReaderType: type) type {
    comptime {
        if (@typeInfo(ReaderType) != .pointer or @typeInfo(ReaderType).pointer.child != std.Io.Reader) {
            @compileError("Utf8Reader expects a pointer to std.Io.Reader");
        }
    }

    return struct {
        const This = @This();

        reader: ReaderType,

        pub fn init(subreader: ReaderType) This {
            return .{ .reader = subreader };
        }

        fn peekBytes(this: *This) !?[]const u8 {
            const ptr = this.reader;
            const greedy = ptr.peekGreedy(1) catch |err| switch (err) {
                error.EndOfStream => return null,
                else => |e| return e,
            };

            const len = try std.unicode.utf8ByteSequenceLength(greedy[0]);
            if (greedy.len >= len) {
                return greedy[0..len];
            }

            const full = try ptr.peek(len);
            return full;
        }

        pub fn peek(this: *This) !?u21 {
            const maybe_bytes = try this.peekBytes();
            if (maybe_bytes) |bytes| {
                return try std.unicode.utf8Decode(bytes);
            }
            return null;
        }

        pub fn read(this: *This) !?u21 {
            const ptr = this.reader;
            const maybe_bytes = try this.peekBytes();
            if (maybe_bytes) |bytes| {
                const codepoint = try std.unicode.utf8Decode(bytes);
                ptr.toss(bytes.len);
                return codepoint;
            }
            return null;
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
            } else if (isAsciiDigit(c)) {
                return .digitChar;
            } else if (isSymbolCharacter(c)) {
                return .symbolChar;
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
            var list = std.ArrayList(u8){};
            errdefer list.deinit(self.allocator);
            var buf: [4]u8 = undefined;

            while (try self.peek()) |c| {
                if (predicate(c)) {
                    const len = try std.unicode.utf8Encode(c, &buf);
                    try list.appendSlice(self.allocator, buf[0..len]);
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

            var list = std.ArrayList(u8){};
            defer list.deinit(self.allocator);

            var buf: [4]u8 = undefined;

            while (true) {
                const c = try self.skip();
                if (c == '"') {
                    break;
                } else if (c == '\\') {
                    switch (try self.skip()) {
                        'n' => try list.appendSlice(self.allocator, "\n"),
                        '"' => try list.appendSlice(self.allocator, "\""),
                        '\\' => try list.appendSlice(self.allocator, "\\"),
                        else => return error.BadEscapeChar,
                    }
                } else {
                    const len = try std.unicode.utf8Encode(c, &buf);
                    try list.appendSlice(self.allocator, buf[0..len]);
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

            var text = try self.readWhile(isSymbolCharacterOrDigit);
            defer text.deinit(self.allocator);

            const uppercase = try toUpperStr(
                self.heap.orb,
                text.items,
            );

            defer self.heap.orb.free(uppercase);

            return self.heap.newSymbol(uppercase, Wisp.nil);
        }

        fn readKeyword(self: *@This()) !u32 {
            try self.skipOnly(':');

            var text = try self.readWhile(isSymbolCharacterOrDigit);
            defer text.deinit(self.allocator);

            const uppercase = try toUpperStr(
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
            var text = try self.readWhile(isSymbolCharacterOrDigit);
            defer text.deinit(self.allocator);

            const uppercase = try toUpperStr(
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
            var str = try self.readWhile(isKeyCharacter);
            defer str.deinit(self.allocator);

            const key = try Keys.parse(str.items);
            const sym = try self.heap.intern(
                &key.toZB32(),
                self.heap.keyPackage,
            );

            try self.heap.set(.sym, .val, sym, sym);

            return sym;
        }

        fn readNumber(self: *@This()) !u32 {
            var str = try self.readWhile(isAsciiDigit);
            defer str.deinit(self.allocator);

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
            var list = std.ArrayList(u32){};
            defer list.deinit(self.allocator);
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
                        try list.append(self.allocator, try self.readValue());
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
            if (isAsciiDigit(c))
                return true;

            return switch (c) {
                '~', '.' => true,
                else => c < 256 and Keys.zb32AlphabetMap[@intCast(c)] >= 0,
            };
        }

        fn isSymbolCharacter(c: u21) bool {
            if (c < 128) switch (c) {
                '(',
                ')',
                '[',
                ']',
                '{',
                '}',
                '"',
                ';',
                '#',
                '`',
                '\'',
                ',',
                => return false,
                else => {
                    const ascii_c: u8 = @as(u8, @truncate(c));
                    return !std.ascii.isWhitespace(ascii_c);
                },
            };
            if (isLetter(c)) return true;
            if (c == '`') return false;
            if (isSymbol(c)) return true;
            if (isEmojiPresentation(c)) return true;
            return switch (c) {
                '/', '_', '-', '!', '%', '&', '*', '?', '@' => true,
                else => false,
            };
        }

        fn isSymbolCharacterOrDigit(c: u21) bool {
            return isSymbolCharacter(c) or isAsciiDigit(c) or c == ':';
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
    var reader_state = std.Io.Reader.fixed(text);
    var reader = makeReader(heap, tmp.get(), &reader_state);
    return reader.readValue();
}

pub fn readMany(heap: *Heap, text: []const u8) !std.ArrayList(u32) {
    var tmp = std.heap.stackFallback(512, heap.orb);
    var list = std.ArrayList(u32){};
    errdefer list.deinit(heap.orb);
    var reader_state = std.Io.Reader.fixed(text);
    var reader = makeReader(heap, tmp.get(), &reader_state);

    while (true) {
        if (try reader.readValueOrEOF()) |x| {
            try list.append(heap.orb, x);
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
