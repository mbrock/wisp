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

pub const PrimopFnTag = enum {
    f0x,

    pub fn from(comptime T: type) PrimopFnTag {
        return switch (T) {
            fn (heap: *Heap, xs: []u32) anyerror!u32 => .f0x,
            else => unreachable,
        };
    }
};

pub const PrimopInfo = struct {
    name: []const u8,
    tag: PrimopFnTag,

    pub fn functionType(self: PrimopInfo) type {
        return switch (self.tag) {
            .f0x => fn (heap: *Heap, xs: []u32) anyerror!u32,
        };
    }

    fn from(comptime T: type, name: []const u8) PrimopInfo {
        return PrimopInfo{
            .name = name,
            .tag = PrimopFnTag.from(T),
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

        var result: u32 = 0;
        for (xs) |x| {
            result += x;
        }

        return result;
    }
};

pub const PrimopInt = wisp.payloadType(wisp.Immediate.primop);
pub const PrimopTag = DeclEnum(Primops, PrimopInt);
pub const primopArray: EnumArray(PrimopTag, Primop) = makePrimopArray();

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
        primopArray.get(.@"+").func,
    );
}
