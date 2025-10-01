// -*- fill-column: 64; -*-
//
// This file is part of Wisp.
//
// Wisp is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version
// 3 of the License, or (at your option) any later version.
//
// Wisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General
// Public License along with Wisp. If not, see
// <https://www.gnu.org/licenses/>.
//

pub const Funs = @import("./jets-fun.zig");
pub const Ctls = @import("./jets-ctl.zig");
pub const Webs = @import("./jets-web.zig");

const Wisp = @import("./wisp.zig");
const Step = @import("./step.zig");

pub const jets = blk: {
    // We generate the boot core without the web jets.  So the
    // boot core contains references to indices in this array,
    // and those need to be stable.  That's why we add the web
    // jets AFTER the basic jets.
    if (Wisp.browser) {
        break :blk makeOpArray(Ctls, .ctl) ++
            makeOpArray(Funs, .fun) ++
            makeOpArray(Webs, .fun);
    } else {
        break :blk makeOpArray(Ctls, .ctl) ++
            makeOpArray(Funs, .fun);
    }
};

const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

const EnumArray = std.enums.EnumArray;

const ref = Wisp.ref;
const Ptr = Wisp.Ptr;

pub const Rest = struct { arg: u32 };

pub const FnTag = enum {
    f0x,
    f0r,
    f0,
    f1,
    f1x,
    f2x,
    f1r,
    f2,
    f3,
    f4,
    f5,

    pub fn from(comptime T: type) FnTag {
        return switch (T) {
            fn (*Step) anyerror!void => .f0,
            fn (*Step, u32) anyerror!void => .f1,
            fn (*Step, u32, u32) anyerror!void => .f2,
            fn (*Step, u32, u32, u32) anyerror!void => .f3,
            fn (*Step, u32, u32, u32, u32) anyerror!void => .f4,
            fn (*Step, u32, u32, u32, u32, u32) anyerror!void => .f5,

            fn (*Step, Rest) anyerror!void => .f0r,
            fn (*Step, u32, Rest) anyerror!void => .f1r,
            fn (*Step, []u32) anyerror!void => .f0x,
            fn (*Step, u32, []u32) anyerror!void => .f1x,
            fn (*Step, u32, u32, []u32) anyerror!void => .f2x,

            else => @compileLog("unhandled op type", T),
        };
    }

    pub fn functionType(comptime self: FnTag) type {
        return switch (self) {
            .f0 => *const fn (*Step) anyerror!void,
            .f1 => *const fn (*Step, u32) anyerror!void,
            .f2 => *const fn (*Step, u32, u32) anyerror!void,
            .f3 => *const fn (*Step, u32, u32, u32) anyerror!void,
            .f4 => *const fn (*Step, u32, u32, u32, u32) anyerror!void,
            .f5 => *const fn (*Step, u32, u32, u32, u32, u32) anyerror!void,

            .f0r => *const fn (*Step, Rest) anyerror!void,
            .f0x => *const fn (*Step, []u32) anyerror!void,
            .f1r => *const fn (*Step, u32, Rest) anyerror!void,
            .f1x => *const fn (*Step, u32, []u32) anyerror!void,
            .f2x => *const fn (*Step, u32, u32, []u32) anyerror!void,
        };
    }

    pub fn cast(comptime this: FnTag, x: anytype) this.functionType() {
        return @as(this.functionType(), @ptrCast(@alignCast(x)));
    }
};

pub const Ilk = enum { fun, ctl };

pub const Op = struct {
    txt: []const u8,
    ilk: Ilk,
    tag: FnTag,
    fun: *const anyopaque,
};

fn makeOpArray(
    comptime S: type,
    comptime ilk: Ilk,
) [std.meta.declarations(S).len]Op {
    const decls = std.meta.declarations(S);
    var ops: [decls.len]Op = undefined;

    var i = 0;
    inline for (decls) |x| {
        const f = @field(S, x.name);
        ops[i] = .{
            .txt = x.name,
            .ilk = ilk,
            .tag = FnTag.from(@TypeOf(f)),
            .fun = f,
        };
        i += 1;
    }

    return ops;
}

test "ops" {
    try expectEqual(jets[0].txt, "QUOTE");
}

pub fn load(heap: *Wisp.Heap) !void {
    inline for (jets, 0..) |jet, i| {
        const sym = try heap.intern(jet.txt, heap.base);
        heap.col(.sym, .fun)[ref(sym)] = Wisp.Imm.make(.jet, i).word();
    }
}
