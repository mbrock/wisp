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
const ziglyph = @import("ziglyph");

const wisp = @import("./ff-wisp.zig");
const Ctx = wisp.Ctx;

const Error = error{
    ReadError,
    EOF,
};

const Reader = @This();

utf8: std.unicode.Utf8Iterator,
ctx: *Ctx,

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
            .symbolChar => try self.readSymbol(),
            .digitChar => try self.readNumber(),
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
    return try self.ctx.new(.duo, .{
        .car = self.ctx.kwd.QUOTE,
        .cdr = try self.ctx.new(.duo, .{
            .car = x,
            .cdr = wisp.nil,
        }),
    });
}

fn readFunctionQuote(self: *Reader) !u32 {
    try self.skipOnly('\'');
    const x = try self.readValue();
    return try self.ctx.new(.duo, .{
        .car = self.ctx.kwd.FUNCTION,
        .cdr = try self.ctx.new(.duo, .{
            .car = x,
            .cdr = wisp.nil,
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
    return wisp.Imm.make(.chr, c).word();
}

fn readUninternedSymbol(self: *Reader) !u32 {
    try self.skipOnly(':');

    const text = try self.readWhile(isSymbolCharacterOrDigit);
    const uppercase = try ziglyph.toUpperStr(
        self.ctx.orb,
        text,
    );

    defer self.ctx.orb.free(uppercase);

    return self.ctx.newSymbol(uppercase, wisp.nil);
}

fn readKeyword(self: *Reader) !u32 {
    try self.skipOnly(':');

    const text = try self.readWhile(isSymbolCharacterOrDigit);
    const uppercase = try ziglyph.toUpperStr(
        self.ctx.orb,
        text,
    );

    defer self.ctx.orb.free(uppercase);

    return try self.ctx.intern(uppercase, self.ctx.keywordPackage);
}

fn readSymbol(self: *Reader) !u32 {
    const text = try self.readWhile(isSymbolCharacterOrDigit);
    const uppercase = try ziglyph.toUpperStr(
        self.ctx.orb,
        text,
    );

    defer self.ctx.orb.free(uppercase);

    return try self.ctx.intern(uppercase, self.ctx.pkg);
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
    return try self.ctx.newv08(text);
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
                return wisp.nil;
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
                return self.ctx.new(.duo, .{
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

fn isSymbolCharacter(c: u21) bool {
    if (ziglyph.isLetter(c)) {
        return true;
    } else {
        return switch (c) {
            '+',
            '-',
            '*',
            '/',
            '@',
            '=',
            '^',
            '%',
            '$',
            '<',
            '>',
            => true,
            else => false,
        };
    }
}

fn isSymbolCharacterOrDigit(c: u21) bool {
    return isSymbolCharacter(c) or ziglyph.isAsciiDigit(c);
}

pub fn read(ctx: *Ctx, stream: []const u8) !u32 {
    var reader = Reader{
        .utf8 = (try std.unicode.Utf8View.init(stream)).iterator(),
        .ctx = ctx,
    };

    return reader.readValue();
}

pub fn readMany(ctx: *Ctx, stream: []const u8) !std.ArrayList(u32) {
    var list = std.ArrayList(u32).init(ctx.orb);

    var reader = Reader{
        .utf8 = (try std.unicode.Utf8View.init(stream)).iterator(),
        .ctx = ctx,
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
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();

    const symbol = try read(&ctx, "foobar");
    const row = try ctx.row(.sym, symbol);
    const name = try ctx.v08slice(row.str);

    try std.testing.expectEqualStrings(
        "FOOBAR",
        name,
    );
}

test "read nil" {
    var ctx = try Ctx.init(std.testing.allocator, .e0);
    defer ctx.deinit();
    try std.testing.expectEqual(wisp.nil, try read(&ctx, "nil"));
}
