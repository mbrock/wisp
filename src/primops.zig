const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const EnumArray = std.enums.EnumArray;

const wisp = @import("./wisp.zig");
const util = @import("./util.zig");

const Heap = wisp.Heap;
const Word = wisp.Word;
const DeclEnum = util.DeclEnum;

pub const FnTag = enum {
    f0x,
    f2,

    pub fn from(comptime T: type) FnTag {
        return switch (T) {
            fn (*Heap, []u32) anyerror!u32 => .f0x,
            fn (*Heap, u32, u32) anyerror!u32 => .f2,
            else => @compileLog("unhandled primop type", T),
        };
    }

    pub fn functionType(comptime self: FnTag) type {
        return switch (self) {
            .f0x => fn (*Heap, []u32) anyerror!u32,
            .f2 => fn (*Heap, u32, u32) anyerror!u32,
        };
    }

    pub fn cast(comptime this: FnTag, x: anytype) this.functionType() {
        return @ptrCast(this.functionType(), x);
    }
};

pub const PrimopInfo = struct {
    name: []const u8,
    tag: FnTag,

    fn from(comptime T: type, name: []const u8) PrimopInfo {
        return PrimopInfo{
            .name = name,
            .tag = FnTag.from(T),
        };
    }
};

pub const Primop = struct {
    info: PrimopInfo,
    func: *const anyopaque,
};

pub const Primfuns = struct {
    pub fn @"+"(heap: *Heap, xs: []u32) anyerror!u32 {
        _ = heap;

        var result: u30 = 0;
        for (xs) |x| {
            result += wisp.decodeFixnum(x);
        }

        return wisp.encodeFixnum(result);
    }
};

pub const Primmacs = struct {
    pub fn @"FOO"(heap: *Heap, x: u32, y: u32) anyerror!u32 {
        _ = heap;
        return try heap.append(.cons, .{
            .car = x,
            .cdr = try heap.append(.cons, .{
                .car = y,
                .cdr = try heap.append(.cons, .{
                    .car = wisp.encodeFixnum(1),
                    .cdr = wisp.NIL,
                }),
            }),
        });
    }
};

pub const PrimfunInt = wisp.payloadType(wisp.Immediate.primfun);
pub const PrimfunTag = DeclEnum(Primfuns, PrimfunInt);

pub const PrimmacInt = wisp.payloadType(wisp.Immediate.primmac);
pub const PrimmacTag = DeclEnum(Primmacs, PrimmacInt);

pub const primfuns = makePrimopArray(PrimfunTag, Primfuns);
pub const primmacs = makePrimopArray(PrimmacTag, Primmacs);

fn makePrimopArray(comptime T: type, comptime S: type) EnumArray(T, Primop) {
    var ops = EnumArray(T, Primop).initUndefined();

    inline for (@typeInfo(T).Enum.fields) |x| {
        const func = @field(S, x.name);
        const info = PrimopInfo.from(@TypeOf(func), x.name);
        ops.set(@intToEnum(T, x.value), .{
            .func = func,
            .info = info,
        });
    }

    return ops;
}

test "primops" {
    try expectEqual(
        @ptrCast(*const anyopaque, Primfuns.@"+"),
        primfuns.get(.@"+").func,
    );
}
