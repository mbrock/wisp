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

pub const Fops = @import("./08-fops.zig");
pub const Mops = @import("./09-mops.zig");

pub const FopTag = DeclEnum(Fops, u27);
pub const MopTag = DeclEnum(Mops, u27);

pub const fops = makeOpArray(FopTag, Fops, .fun);
pub const mops = makeOpArray(MopTag, Mops, .ctl);

const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const EnumArray = std.enums.EnumArray;

const wisp = @import("./ff-wisp.zig");
const Eval = @import("./04-eval.zig");
const util = @import("./00-util.zig");

const ref = wisp.ref;
const Job = Eval;
const Ptr = wisp.Ptr;
const DeclEnum = util.DeclEnum;

pub const Rest = struct { arg: u32 };

pub const FnTag = enum {
    fc,
    f0x,
    f0r,
    f0,
    f1,
    f2,
    f3,

    pub fn from(comptime T: type) FnTag {
        return switch (T) {
            fn (*Job, u32) anyerror!void => .fc,

            fn (*Job) anyerror!u32 => .f0,
            fn (*Job, u32) anyerror!u32 => .f1,
            fn (*Job, u32, u32) anyerror!u32 => .f2,
            fn (*Job, u32, u32, u32) anyerror!u32 => .f3,

            fn (*Job, Rest) anyerror!u32 => .f0r,
            fn (*Job, []u32) anyerror!u32 => .f0x,

            else => @compileLog("unhandled op type", T),
        };
    }

    pub fn functionType(comptime self: FnTag) type {
        return switch (self) {
            .fc => fn (*Job, u32) anyerror!void,

            .f0 => fn (*Job) anyerror!u32,
            .f1 => fn (*Job, u32) anyerror!u32,
            .f2 => fn (*Job, u32, u32) anyerror!u32,
            .f3 => fn (*Job, u32, u32, u32) anyerror!u32,

            .f0r => fn (*Job, Rest) anyerror!u32,
            .f0x => fn (*Job, []u32) anyerror!u32,
        };
    }

    pub fn cast(comptime this: FnTag, x: anytype) this.functionType() {
        return @ptrCast(this.functionType(), x);
    }
};

pub const Ilk = enum { fun, mac, ctl };

pub const Op = struct {
    txt: []const u8,
    ilk: Ilk,
    tag: FnTag,
    fun: *const anyopaque,
};

fn makeOpArray(
    comptime T: type,
    comptime S: type,
    ilk: Ilk,
) EnumArray(T, Op) {
    var ops = EnumArray(T, Op).initUndefined();

    inline for (@typeInfo(T).Enum.fields) |x| {
        const f = @field(S, x.name);
        ops.set(@intToEnum(T, x.value), .{
            .txt = x.name,
            .ilk = ilk,
            .tag = FnTag.from(@TypeOf(f)),
            .fun = f,
        });
    }

    return ops;
}

test "ops" {
    try expectEqual(
        @ptrCast(*const anyopaque, Fops.@"+"),
        fops.get(.@"+").fun,
    );
}

pub fn load(ctx: *wisp.Ctx) !void {
    inline for (fops.values) |fop, i| {
        var sym = try ctx.intern(fop.txt, ctx.base);
        ctx.col(.sym, .fun)[ref(sym)] = wisp.Imm.make(.fop, i).word();
    }

    inline for (mops.values) |mop, i| {
        var sym = try ctx.intern(mop.txt, ctx.base);
        ctx.col(.sym, .fun)[ref(sym)] = wisp.Imm.make(.mop, i).word();
    }
}
