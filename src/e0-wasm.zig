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

const std = @import("std");

const eval = @import("./04-eval.zig");
const xops = @import("./07-xops.zig");
const wisp = @import("./ff-wisp.zig");

pub fn main() void {}

export const wisp_tag_int = wisp.Tag.int;
export const wisp_tag_sys = wisp.Tag.sys;
export const wisp_tag_chr = wisp.Tag.chr;
export const wisp_tag_jet = wisp.Tag.jet;
export const wisp_tag_duo = wisp.Tag.duo;
export const wisp_tag_sym = wisp.Tag.sym;
export const wisp_tag_fun = wisp.Tag.fun;
export const wisp_tag_mac = wisp.Tag.mac;
export const wisp_tag_v32 = wisp.Tag.v32;
export const wisp_tag_v08 = wisp.Tag.v08;
export const wisp_tag_pkg = wisp.Tag.pkg;
export const wisp_tag_ktx = wisp.Tag.ktx;
export const wisp_tag_bot = wisp.Tag.bot;

export const wisp_sys_t: u32 = wisp.t;
export const wisp_sys_nil: u32 = wisp.nil;
export const wisp_sys_nah: u32 = wisp.nah;
export const wisp_sys_zap: u32 = wisp.zap;
export const wisp_sys_top: u32 = wisp.top;

export fn wisp_heap_init() ?*wisp.Heap {
    var orb = std.heap.page_allocator;

    if (orb.create(wisp.Heap)) |heapptr| {
        if (wisp.Heap.init(orb, .e0)) |heap| {
            var heap2 = heap;
            xops.load(&heap2) catch return null;
            heap2.cook() catch return null;
            heapptr.* = heap2;
            return heapptr;
        } else |_| {
            return null;
        }
    } else |_| {
        return null;
    }
}

export fn wisp_read(heap: *wisp.Heap, str: [*:0]const u8) u32 {
    if (wisp.read(heap, std.mem.span(str))) |x| {
        return x;
    } else |_| {
        return wisp.zap;
    }
}

export fn wisp_eval(heap: *wisp.Heap, exp: u32, max: u32) u32 {
    var bot = eval.initBot(exp);
    return eval.init(heap, &bot).evaluate(max, false) catch wisp.zap;
}

export fn wisp_bot_init(heap: *wisp.Heap, exp: u32) u32 {
    return heap.new(.bot, .{
        .way = wisp.top,
        .env = wisp.nil,
        .err = wisp.nil,
        .val = wisp.nah,
        .exp = exp,
    }) catch wisp.zap;
}

export fn wisp_eval_step(heap: *wisp.Heap, botptr: u32) u32 {
    var bot = heap.row(.bot, botptr) catch return wisp.zap;
    var exe = eval.init(heap, &bot);

    exe.step() catch return wisp.zap;

    heap.put(.bot, botptr, bot) catch return wisp.zap;

    return if (bot.val != wisp.nah and bot.way == wisp.top)
        wisp.nil
    else
        wisp.t;
}

fn Field(comptime name: []const u8, t: type) std.builtin.TypeInfo.StructField {
    return .{
        .name = name,
        .field_type = t,
        .default_value = null,
        .is_comptime = false,
        .alignment = 0,
    };
}

fn TabDat(tag: wisp.Tag) type {
    const n = std.meta.fields(wisp.Row(tag)).len;
    var fields: [1 + n]std.builtin.TypeInfo.StructField = undefined;

    fields[0] = Field("n", u32);

    for (std.meta.fields(wisp.Row(tag))) |field, i| {
        fields[1 + i] = Field(field.name, usize);
    }

    const t = @Type(std.builtin.TypeInfo{
        .Struct = .{
            .layout = .Packed,
            .fields = &fields,
            .decls = &[_]std.builtin.TypeInfo.Declaration{},
            .is_tuple = false,
        },
    });

    return t;
}

const Dat = packed struct {
    duo: TabDat(.duo),
    sym: TabDat(.sym),
    fun: TabDat(.fun),
    mac: TabDat(.mac),
    v08: TabDat(.v08),
    v32: TabDat(.v32),
    pkg: TabDat(.pkg),
    ktx: TabDat(.ktx),
    bot: TabDat(.bot),
};

export fn wisp_dat_init(heap: *wisp.Heap) ?*Dat {
    return heap.orb.create(Dat) catch null;
}

export fn wisp_dat_read(heap: *wisp.Heap, dat: *Dat) void {
    inline for (wisp.pointerTags) |tag| {
        const E = std.meta.FieldEnum(wisp.Row(tag));
        const tab = heap.tab(tag);
        var tagdat = &@field(dat, @tagName(tag));
        tagdat.n = @intCast(u32, tab.list.len);
        inline for (std.meta.fields(wisp.Row(tag))) |field, i| {
            const slice = tab.list.slice();
            @field(tagdat, field.name) = @ptrToInt(slice.items(@intToEnum(E, i)).ptr);
        }
    }
}

export fn wisp_heap_v08_len(heap: *wisp.Heap) usize {
    return heap.v08.items.len;
}

export fn wisp_heap_v08_ptr(heap: *wisp.Heap) [*]u8 {
    return heap.v08.items.ptr;
}

export fn wisp_heap_v32_len(heap: *wisp.Heap) usize {
    return heap.v32.list.items.len;
}

export fn wisp_heap_v32_ptr(heap: *wisp.Heap) [*]u32 {
    return heap.v32.list.items.ptr;
}

export fn wisp_alloc(heap: *wisp.Heap, n: u32) usize {
    const buf = heap.orb.alloc(u8, n) catch return 0;
    return @ptrToInt(buf.ptr);
}

export fn wisp_free(heap: *wisp.Heap, x: [*:0]u8) void {
    heap.orb.free(std.mem.span(x));
}

export fn wisp_destroy(heap: *wisp.Heap, x: [*]u8) void {
    heap.orb.destroy(x);
}

test "sanity" {
    try std.testing.expectEqual(0x88000000, wisp_sys_nil);
}
