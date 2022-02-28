const std = @import("std");
const ziglyph = @import("ziglyph");

const wisp = @import("./wisp.zig");
const Vat = wisp.Vat;

const Error = error{
    ReadError,
    EOF,
};

const Reader = @This();

utf8: std.unicode.Utf8Iterator,
vat: *Vat,

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
        self.vat.orb,
        text,
    );

    defer self.vat.orb.free(uppercase);

    return try self.vat.intern(uppercase, self.vat.base());
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
    return try self.vat.newstr(text);
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

                return self.vat.new(.duo, .{
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

pub fn read(vat: *Vat, stream: []const u8) !u32 {
    var reader = Reader{
        .utf8 = (try std.unicode.Utf8View.init(stream)).iterator(),
        .vat = vat,
    };

    return reader.readValue();
}

test "read symbol uppercasing" {
    var vat = try Vat.init(std.testing.allocator, .e0);
    defer vat.deinit();

    const symbol = try read(&vat, "foobar");
    const symbolData = try vat.get(.sym, symbol);
    const symbolName = try vat.strslice(symbolData.str);

    try std.testing.expectEqualStrings(
        "FOOBAR",
        symbolName,
    );
}
