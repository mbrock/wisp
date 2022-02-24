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

pub const E = enum { e, n };
pub const EW = struct { w: u32 };
pub const NW = struct { w: u32 };

pub const PrimopKind = struct {
    name: []const u8,
    vararg: bool,
    args: u5,
    eval: std.StaticBitSet(32),
};

pub const Primop = struct {
    func: *const anyopaque,
    kind: PrimopKind,
};

pub const Primops = struct {
    pub fn @"+"(heap: *Heap, xs: []const EW) NW {
        _ = heap;

        var result: u32 = 0;
        for (xs) |x| {
            result += x.w;
        }

        return .{ .w = result };
    }
};

fn primopKindOf(comptime f: type, name: []const u8) PrimopKind {
    const info = @typeInfo(f).Fn;
    const args = info.args[1..info.args.len];

    var eval = std.StaticBitSet(32).initEmpty();
    inline for (args) |arg, i| {
        const t = arg.arg_type;
        if (t == EW or t == []const EW) {
            eval.set(i + 1);
        }
    }

    if (info.return_type == EW) {
        eval.set(0);
    }

    const lastArgType = args[args.len - 1].arg_type orelse void;

    return PrimopKind{
        .name = name,
        .args = args.len,
        .eval = eval,
        .vararg = switch (lastArgType) {
            []const EW,
            []const NW,
            => true,
            else => false,
        },
    };
}

pub const PrimopInt = wisp.payloadType(wisp.Immediate.primop);
pub const PrimopTag = DeclEnum(Primops, PrimopInt);
pub const primopArray: EnumArray(PrimopTag, Primop) = makePrimopArray();

fn makePrimopArray() EnumArray(PrimopTag, Primop) {
    var ops = EnumArray(PrimopTag, Primop).initUndefined();

    inline for (@typeInfo(PrimopTag).Enum.fields) |x| {
        const func = @field(Primops, x.name);
        const kind = primopKindOf(@TypeOf(func), x.name);
        ops.set(@intToEnum(PrimopTag, x.value), .{
            .func = func,
            .kind = kind,
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
