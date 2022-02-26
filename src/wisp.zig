///
/// |----------+-------+-----+------------------------------------|
/// | Class    | Octal | Hex | Binary                             |
/// |----------+-------+-----+------------------------------------|
/// | fixnum   |     0 |   0 | ~xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx00~ |
/// | nil      |     1 |   1 | ~00000000000000000000000000000001~ |
/// | cons     |     2 |   2 | ~xxxxxxxxxxxxxxxxxxxxxxxxxxxxx010~ |
/// | symbol   |     3 |   3 | ~xxxxxxxxxxxxxxxxxxxxxxxxxxxxx011~ |
/// | glyph    |     5 |   5 | ~xxxxxxxxxxxxxxxxxxxxxxxxxxxxx101~ |
/// | string   |     6 |   6 | ~xxxxxxxxxxxxxxxxxxxxx00000000110~ |
/// | zap      |     7 |   7 | ~xxxxxxxxxxxxxxxxxxxxx00000000111~ |
/// | package  |    17 |   f | ~xxxxxxxxxxxxxxxxxxxxx00000001111~ |
/// | primfun  |    27 |  17 | ~xxxxxxxxxxxxxxxxxxxxx00000010111~ |
/// | primmac  |    37 |  1f | ~xxxxxxxxxxxxxxxxxxxxx00000011111~ |
/// | callplan |    47 |  27 | ~xxxxxxxxxxxxxxxxxxxxx00000101111~ |
/// |----------+-------+-----+------------------------------------|
///
pub const T0 = enum(u11) {
    fixnum = 0,
    nil = 1,
    cons = 2,
    symbol = 3,
    glyph = 5,
    string = 6,
    zap = 7,
    package = 0o17,
    primfun = 0o27,
    primmac = 0o37,
    argsplan = 0o47,

    pub fn tag(comptime t: T0) t.tagType() {
        return switch (t) {
            .argsplan => 0b100111,
            .cons => 0b010,
            .fixnum => 0,
            .glyph => 0b111,
            .nil => NIL,
            .package => 0b10111,
            .primmac => 0b10111,
            .primfun => 0b11111,
            .string => 0b110,
            .symbol => 0b001,
            .zap => ZAP,
        };
    }

    pub fn valueType(t: T0) type {
        return switch (t) {
            .argsplan => ArgsPlan,
            .cons => Cons,
            .fixnum => u30,
            .glyph => u21,
            .nil => u0,
            .package => Package,
            .string => String,
            .symbol => Symbol,
            .zap => u0,
        };
    }

    pub fn tagBits(comptime t: T0) comptime_int {
        return switch (t) {
            .fixnum => 2,

            .cons,
            .string,
            .symbol,
            => 3,

            .argsPlan,
            .glyph,
            .package,
            .primfun,
            .primmac,
            => 11,

            .nil,
            .zap,
            => 32,
        };
    }

    pub fn payloadBits(comptime t: T0) comptime_int {
        return 32 - t.tagBits();
    }

    pub fn payloadType(comptime t: T0) comptime_int {
        return Uint(t.payloadBits());
    }

    pub fn tagType(comptime t: T0) comptime_int {
        return Uint(t.tagBits());
    }

    pub fn containerType(comptime t: T0) type {
        return switch (t) {
            .argsplan,
            .cons,
            .package,
            .string,
            .symbol,
            => std.MultiArrayList(t.valueType()),

            .fixnum,
            .glyph,
            .nil,
            .primfun,
            .primmac,
            .zap,
            => void,
        };
    }

    pub fn tagify(comptime t: T0, x: t.payloadType()) Word {
        return .{
            .raw = (@intCast(u32, x) << t.tagBits()) | t.tag(),
        };
    }
};

pub const Word = struct {
    raw: u32,

    pub const zap = Word.from(ZAP);
    pub const nil = Word.from(NIL);

    pub fn tag(word: Word) T0 {
        const x = word.raw;

        if (x == NIL) {
            return .nil;
        }

        if (x == ZAP) {
            return .zap;
        }

        return switch (@intCast(u3, x & 0b111)) {
            0b000 => .fixnum,
            0b001 => .symbol,
            0b010 => .cons,
            0b011 => unreachable,
            0b100 => .fixnum,
            0b101 => unreachable,
            0b110 => .string,
            0b111 => switch (x & ~@as(u11, 0)) {
                0b000111 => .glyph,
                0b010111 => .package,
                0b011111 => .primfun,
                0b101111 => .primmac,
                0b100111 => .argsPlan,
                else => {
                    std.log.warn("weird {b}", .{x});
                    unreachable;
                },
            },
        };
    }

    pub fn eql(x: Word, y: Word) bool {
        return x.raw == y.raw;
    }

    pub fn semispace(x: Word) Semispace {
        return Semispace.of(x.raw);
    }
};

test "tag bit stuff" {
    try expectEqual(u29, T0.cons.payloadType());
    try expectEqual(u3, T0.cons.tagType());

    const x = T0.cons.tagify(0b100000000000000000000000000000);
    try expectEqual(@as(u32, 0b10000000000000000000000000000010), x.raw);
    try expectEqual(x.semispace(), Semispace.space1);
}

pub const String = struct {
    offset: u32,
    length: u32,
};

pub const Symbol = struct {
    name: u32,
    package: u32,
    value: u32 = ZAP,
    function: u32 = NIL,
};

pub const Package = struct {
    name: u32,
    symbols: u32 = NIL,
};

pub const Cons = struct {
    car: u32,
    cdr: u32,
};

pub const ArgsPlan = struct {
    next: u32,
    scopes: u32,
    callee: u32,
    values: u32,
    terms: u32,
};

fn SliceOf(comptime t: type) type {
    return std.MultiArrayList(t).Slice;
}

pub fn EnumFieldContainers(comptime E: type) type {
    const StructField = std.builtin.TypeInfo.StructField;
    var fields: []const StructField = &[_]StructField{};
    inline for (std.meta.fields(E)) |field| {
        const e = @intToEnum(E, field.value);
        const fieldType = e.containerType();
        fields = fields ++ &[_]StructField{.{
            .name = field.name,
            .field_type = fieldType,
            .default_value = @as(fieldType, e.emptyContainer()),
            .is_comptime = false,
            .alignment = @alignOf(fieldType),
        }};
    }

    return @Type(.{ .Struct = .{
        .layout = .Auto,
        .fields = fields,
        .decls = &[_]std.builtin.TypeInfo.Declaration{},
        .is_tuple = false,
    } });
}

pub const Heap = struct {
    gpa: std.mem.Allocator,
    semispace: Semispace = .space0,
    stringBytes: std.ArrayListUnmanaged(u8) = .{},
    data: EnumFieldContainers(T0) = .{},

    pub fn kindContainer(
        this: *Heap,
        comptime t: T0,
    ) *t.containerType() {
        return &@field(this.data, @tagName(t));
    }

    pub fn init(gpa: std.mem.Allocator) !Heap {
        var this = Heap{
            .gpa = gpa,
        };

        try this.data.package.append(gpa, Package{
            .name = try this.addString("WISP"),
        });

        return this;
    }

    pub fn loadPrimops(this: *Heap) !void {
        inline for (Primops.primfuns.values) |primop, i| {
            const symbol = try this.internStringInBasePackage(primop.info.name);
            const function = try this.symbolFunction(symbol);
            function.* = T0.primfun.tagify(i);
        }

        inline for (Primops.primmacs.values) |primop, i| {
            const symbol = try this.internStringInBasePackage(primop.info.name);
            const function = try this.symbolFunction(symbol);
            assert(function.* == NIL);
            function.* = T0.primmac.tagify(i);
        }
    }

    pub fn alloc(this: *Heap, comptime t: T0, x: t.valueType()) !t.payloadType() {
        var container = this.kindContainer(t);
        const i = @intCast(t.payloadType(), container.len);
        try container.append(this.gpa, x);
        return i;
    }

    pub fn append(this: *Heap, comptime t: T0, x: t.valueType()) !u32 {
        const i = try this.alloc(t, x);
        const p = t.tagify(i);
        if (this.semispace == .space1) {
            return p | Semispace.mask;
        } else {
            return p;
        }
    }

    pub fn deinit(this: *Heap) void {
        inline for (std.meta.fields(T0)) |field| {
            @field(this.data, field.name).deinit(this.gpa);
        }

        this.stringBytes.deinit(this.gpa);
        this.* = .{ .gpa = this.gpa };
    }

    pub fn allocString(this: *Heap, text: []const u8) !u29 {
        const offset = @intCast(u30, this.stringBytes.items.len);
        const length = @intCast(u30, text.len);

        try this.stringBytes.appendSlice(this.gpa, text);
        return this.alloc(.string, .{
            .offset = encodeFixnum(offset),
            .length = encodeFixnum(length),
        });
    }

    pub fn addString(this: *Heap, text: []const u8) !u32 {
        const idx = try this.allocString(text);
        return this.makePointer(.{ .string = idx });
    }

    pub fn stringSlice(this: *const Heap, ptr: u32) []const u8 {
        const idx = Word.from(ptr).pointer.offset(.string, this.semispace);
        const string: String = this.data.string.get(idx);
        const offset0 = decodeFixnum(string.offset);
        const offset1 = offset0 + decodeFixnum(string.length);
        return this.stringBytes.items[offset0..offset1];
    }

    pub fn internStringInBasePackage(this: *Heap, name: []const u8) !u32 {
        return this.internString(name, this.basePackage());
    }

    pub fn basePackage(this: *Heap) u32 {
        const x = this.makePointer(.{ .package = 0 });
        return x;
    }

    pub fn internString(
        this: *Heap,
        name: []const u8,
        package: u32,
    ) !u32 {
        const packageIdx = Word.from(package).pointer.offset(.package, this.semispace);
        var symbols = &this.data.package.items(.symbols)[packageIdx];
        const symbolNames = this.data.symbol.items(.name);

        var cur = symbols.*;
        while (cur != NIL) {
            const it = try this.car(cur);
            const symbolIdx = Word.from(it).pointer.offset(.symbol, this.semispace);
            const itsName = symbolNames[symbolIdx];
            const s = this.stringSlice(itsName);

            if (std.mem.eql(u8, s, name)) {
                return it;
            } else {
                cur = try this.cdr(cur);
            }
        }

        const stringIdx = try this.allocString(name);
        const symbolIdx = @intCast(u29, this.data.symbol.len);

        try this.data.symbol.append(this.gpa, .{
            .name = this.makePointer(.{ .string = stringIdx }),
            .package = packageIdx,
        });

        const ptr = this.makePointer(.{ .symbol = symbolIdx });

        symbols.* = try this.append(.cons, .{
            .car = ptr,
            .cdr = symbols.*,
        });

        return ptr;
    }

    pub fn symbolValue(this: *Heap, ptr: u32) !*u32 {
        const i = this.getOffset(.symbol, ptr);
        return &this.data.symbol.items(.value)[i];
    }

    pub fn symbolFunction(this: *Heap, ptr: u32) !*u32 {
        const i = this.getOffset(.symbol, ptr);
        return &this.data.symbol.items(.function)[i];
    }

    fn pointerWord(this: *const Heap, ptr: u32) Word {
        const word = Word.from(ptr);
        assert(word.pointer.semispace() == this.semispace);
        return word;
    }

    pub fn deref(
        this: *Heap,
        comptime kind: T0,
        x: u32,
    ) !kind.valueType() {
        return this.kindContainer(kind).get(this.getOffset(kind, x));
    }

    pub fn get(
        this: *Heap,
        comptime kind: T0,
        i: kind.payloadType(),
    ) !kind.valueType() {
        return this.kindContainer(kind).get(i);
    }

    pub fn getField(
        this: *Heap,
        comptime kind: T0,
        comptime field: kind.containerType().Field,
        i: kind.payloadType(),
    ) !std.meta.fieldInfo(kind.valueType(), field).field_type {
        return this.kindContainer(kind).items(field)[i];
    }

    pub fn getOffset(this: *Heap, comptime kind: T0, x: u32) payloadType(kind) {
        return Word.from(x).pointer.offset(kind, this.semispace);
    }

    pub fn car(this: *Heap, ptr: u32) !u32 {
        return (try this.deref(.cons, ptr)).car;
    }

    pub fn cdr(this: *Heap, ptr: u32) !u32 {
        return (try this.deref(.cons, ptr)).cdr;
    }

    pub fn pointerDeref(
        this: *Heap,
        comptime kind: Kind,
        p: Pointer,
    ) !kind.valueType() {
        return this.kindContainer(kind).get(
            p.offset(kind, this.semispace),
        );
    }

    pub fn list(this: *Heap, xs: anytype) !u32 {
        var result = NIL;
        var i: usize = 0;
        while (i < xs.len) : (i += 1) {
            result = try this.append(.cons, .{
                .car = xs[xs.len - i - 1],
                .cdr = result,
            });
        }

        return result;
    }
};

pub const Semispace = enum(u1) {
    space0 = 0,
    space1 = 1,

    pub const mask = @as(u32, 1) << 31;

    pub fn other(this: Semispace) Semispace {
        return switch (this) {
            .space0 => .space1,
            .space1 => .space0,
        };
    }

    pub fn word(this: Semispace) u32 {
        return switch (this) {
            .space0 => 0,
            .space1 => @as(u32, 1) << 31,
        };
    }

    pub fn makePointer(semispace: Semispace, x: Pointer) u32 {
        return semispace.word() | x.raw();
    }

    pub fn of(x: u32) Semispace {
        return if (x & mask == 0) .space0 else .space1;
    }
};

pub fn encodeFixnum(x: u30) u32 {
    return x << 2;
}

pub fn decodeFixnum(x: u32) u30 {
    return @intCast(u30, x >> 2);
}

fn expectParsingRoundtrip(text: []const u8) !void {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    var list = std.ArrayList(u8).init(std.testing.allocator);
    defer list.deinit();
    const writer = list.writer();

    const x = try read(&heap, text);
    // try dumpDataStderr(&heap);

    try print(&heap, &writer, x);
    try std.testing.expectEqualStrings(text, list.items);
}

pub fn dumpMultiArrayList(out: anytype, name: []const u8, x: anytype) !void {
    try out.print("* {s}\n", .{name});
    var i: usize = 0;
    while (i < x.len) : (i += 1) {
        try out.print("  - {any}\n", .{x.get(i)});
    }
}

pub fn dumpArrayList(out: anytype, name: []const u8, x: anytype) !void {
    try out.print("* {s}\n", .{name});
    var i: usize = 0;
    while (i < x.items.len) : (i += 1) {
        try out.print("  - {s}\n", .{x.items[i]});
    }
}

pub fn dumpHashMap(out: anytype, name: []const u8, x: anytype) !void {
    try out.print("* {s}\n", .{name});
    var it = x.iterator();
    while (it.next()) |kv| {
        try out.print("  {any} = {any}\n", .{
            kv.key_ptr.*,
            kv.value_ptr.*,
        });
    }
}

pub fn dumpDataStderr(this: *Heap) !void {
    try dumpData(this, std.io.getStdErr().writer());
}

pub fn dumpData(this: *Heap, out: anytype) !void {
    try dumpMultiArrayList(out, "symbols", this.data.symbol);
    try dumpMultiArrayList(out, "packages", this.data.package);
    try dumpMultiArrayList(out, "conses", this.data.cons);
    try dumpMultiArrayList(out, "strings", this.data.string);

    try out.print(
        "* stringBytes\n  {s}\n",
        .{this.stringBytes.items},
    );
}

test {
    std.testing.refAllDecls(@This());
}

test "initialize wisp" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();
}

test "intern string" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    const x = Word.from(
        try heap.internStringInBasePackage("FOO"),
    );

    try expectEqual(@as(u29, 0), x.pointer.symbol);
}

test "cons" {
    var heap = try Heap.init(std.testing.allocator);
    defer heap.deinit();

    const x = try heap.append(.cons, .{
        .car = encodeFixnum(1),
        .cdr = encodeFixnum(2),
    });

    try expectEqual(encodeFixnum(1), try heap.car(x));
    try expectEqual(encodeFixnum(2), try heap.cdr(x));
}

test "fixnum lowtag" {
    try expectEqual(
        Word{ .immediate = .{ .fixnum = 0 } },
        Word.from(encodeFixnum(0)),
    );

    try expectEqual(
        Word{ .immediate = .{ .fixnum = 1 } },
        Word.from(encodeFixnum(1)),
    );
}

test "fixnum roundtrip" {
    try expectEqual(@as(u30, 123), decodeFixnum(encodeFixnum(123)));
}

test "read roundtrips" {
    try expectParsingRoundtrip("NIL");
    try expectParsingRoundtrip("123");
    try expectParsingRoundtrip("FOO");
    try expectParsingRoundtrip("FÖÖ");
    try expectParsingRoundtrip("\"Hello, world!\"");
}

test "read list roundtrips" {
    try expectParsingRoundtrip("(FOO)");
    try expectParsingRoundtrip("(FOO (1 2 3) BAR)");
    try expectParsingRoundtrip("(1 . 2)");
}

test "read code roundtrips" {
    try expectParsingRoundtrip("(DEFUN FOO (X Y) (+ X Y))");
}
