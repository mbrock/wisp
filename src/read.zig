const std = @import("std");
const ziglyph = @import("ziglyph");

const wisp = @import("./wisp.zig");

const Error = error{
    ReadError,
    EOF,
};

const Reader = @This();

utf8: std.unicode.Utf8Iterator,
data: *wisp.Data,

fn readValue(self: *Reader) anyerror!u32 {
    try self.skipSpace();

    const next = try self.peek();
    if (next) |c| {
        return switch (try classifyInitial(c)) {
            .leftParen => self.readList(),
            .doubleQuote => self.readString(),
            .symbolChar => self.readSymbol(),
            .digitChar => self.readNumber(),
        };
    } else {
        return Error.ReadError;
    }
}

const InitialCharType = enum {
    leftParen,
    symbolChar,
    digitChar,
    doubleQuote,
};

fn classifyInitial(c: u21) !InitialCharType {
    if (c == '(') {
        return .leftParen;
    } else if (isSymbolCharacter(c)) {
        return .symbolChar;
    } else if (ziglyph.isAsciiDigit(c)) {
        return .digitChar;
    } else if (c == '"') {
        return .doubleQuote;
    } else {
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

    return text;
}

fn readSymbol(self: *Reader) !u32 {
    const text = try self.readWhile(isSymbolCharacter);
    const uppercase = try ziglyph.toUpperStr(
        self.data.gpa,
        text,
    );

    defer self.data.gpa.free(uppercase);

    return try self.data.internString(uppercase, 0);
}

fn readNumber(self: *Reader) !u32 {
    const numberText = try self.readWhile(ziglyph.isAsciiDigit);

    var result: u30 = 0;
    var magnitude = std.math.pow(
        u30,
        10,
        @intCast(u30, numberText.len - 1),
    );
    for (numberText) |c| {
        result += magnitude * (c - '0');
        magnitude /= 10;
    }

    return wisp.encodeFixnum(result);
}

fn readString(self: *Reader) !u32 {
    try self.skipOnly('"');
    const text = try self.readWhile(isNotEndOfString);
    try self.skipOnly('"');
    return self.data.addString(text);
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
                return wisp.NIL;
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

                return self.data.addCons(.{
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
    return self.utf8.nextCodepoint().?;
}

fn skipSpace(self: *Reader) !void {
    while (try self.peek()) |c| {
        switch (c) {
            ' ', '\n' => {
                _ = try self.skip();
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
            '+', '-', '*', '/', '@', '=', '^', '%', '$' => true,
            else => false,
        };
    }
}

pub fn read(data: *wisp.Data, stream: []const u8) !u32 {
    var reader = Reader{
        .utf8 = (try std.unicode.Utf8View.init(stream)).iterator(),
        .data = data,
    };

    return reader.readValue();
}

test "read symbol uppercasing" {
    var data = try wisp.Data.init(std.testing.allocator);
    defer data.deinit();

    const symbol = try read(&data, "foobar");
    const symbolData = try data.symbol(symbol);
    const symbolName = data.strings.items[symbolData.name];

    try std.testing.expectEqualStrings(
        "FOOBAR",
        symbolName,
    );
}
