const std = @import("std");
const ziglyph = @import("ziglyph");

const wisp = @import("./base.zig");
const W = wisp.W;

const Reader = struct {
    utf8: std.unicode.Utf8Iterator,
    wisp: *wisp.Wisp,

    fn read(self: *Reader) anyerror!W {
        try self.skipSpace();

        const next = try self.peek();
        if (next) |c| {
            if (c == '(') {
                return self.readList();
            } else if (isSymbolCharacter(c)) {
                return self.readSymbol();
            } else if (ziglyph.isAsciiDigit(c)) {
                return self.readNumber();
            } else if (c == '"') {
                return self.readString();
            } else {
                return wisp.Error.ReadError;
            }
        } else {
            return wisp.Error.ReadError;
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

    fn readSymbol(self: *Reader) !W {
        const text = try self.readWhile(isSymbolCharacter);
        const uppercase = try ziglyph.toUpperStr(
            self.wisp.heap.allocator,
            text,
        );

        defer self.wisp.heap.allocator.free(uppercase);

        const symbol = try self.wisp.internString(
            uppercase,
            self.wisp.basePackage,
        );

        return symbol;
    }

    fn readNumber(self: *Reader) !W {
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

        return wisp.fixnum(result);
    }

    fn readString(self: *Reader) !W {
        try self.skipOnly('"');

        const text = try self.readWhile(isNotEndOfString);

        return wisp.makeString(&self.wisp.heap, text);
    }

    fn readList(self: *Reader) !W {
        try self.skipOnly('(');
        return self.readListTail();
    }

    fn readListTail(self: *Reader) anyerror!W {
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

                    const cdr = try self.read();

                    try self.skipSpace();
                    try self.skipOnly(')');

                    return cdr;
                },

                else => {
                    const car = try self.read();
                    const cdr = try self.readListTail();

                    return self.wisp.cons(car, cdr);
                },
            }
        } else {
            return wisp.Error.EOF;
        }
    }

    fn skipOnly(self: *Reader, c: u21) !void {
        if ((try self.peek()) != c) {
            return wisp.Error.ReadError;
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
};

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

pub fn read(ctx: *wisp.Wisp, stream: []const u8) !W {
    var reader = Reader{
        .utf8 = (try std.unicode.Utf8View.init(stream)).iterator(),
        .wisp = ctx,
    };

    return reader.read();
}

test "read symbol uppercasing" {
    var ctx = try wisp.testWisp();
    defer ctx.heap.free();

    const symbol = try read(&ctx, "foobar");
    const data = try ctx.getSymbolData(symbol);

    try std.testing.expectEqualStrings(
        "FOOBAR",
        try ctx.stringBufferAsSlice(data.name),
    );
}
