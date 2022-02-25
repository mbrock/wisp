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

    pub fn from(comptime T: type) FnTag {
        return switch (T) {
            fn (heap: *Heap, xs: []u32) anyerror!u32 => .f0x,
            else => unreachable,
        };
    }

    pub fn functionType(comptime self: FnTag) type {
        return switch (self) {
            .f0x => fn (heap: *Heap, xs: []u32) anyerror!u32,
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

pub const Primops = struct {
    pub fn @"+"(heap: *Heap, xs: []u32) anyerror!u32 {
        _ = heap;

        var result: u30 = 0;
        for (xs) |x| {
            result += wisp.decodeFixnum(x);
        }

        return wisp.encodeFixnum(result);
    }
};

pub const PrimopInt = wisp.payloadType(wisp.Immediate.primop);
pub const PrimopTag = DeclEnum(Primops, PrimopInt);
pub const array: EnumArray(PrimopTag, Primop) = makePrimopArray();

fn makePrimopArray() EnumArray(PrimopTag, Primop) {
    var ops = EnumArray(PrimopTag, Primop).initUndefined();

    inline for (@typeInfo(PrimopTag).Enum.fields) |x| {
        const func = @field(Primops, x.name);
        const info = PrimopInfo.from(@TypeOf(func), x.name);
        ops.set(@intToEnum(PrimopTag, x.value), .{
            .func = func,
            .info = info,
        });
    }

    return ops;
}

test "primops" {
    try expectEqual(
        @ptrCast(*const anyopaque, Primops.@"+"),
        array.get(.@"+").func,
    );
}
