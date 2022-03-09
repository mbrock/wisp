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

const Wisp = @import("./wisp.zig");
const Read = @import("./sexp-read.zig");
const Step = @import("./step.zig");
const Jets = @import("./jets.zig");

pub fn main() void {}

export const wisp_tag_int = Wisp.Tag.int;
export const wisp_tag_sys = Wisp.Tag.sys;
export const wisp_tag_chr = Wisp.Tag.chr;
export const wisp_tag_jet = Wisp.Tag.jet;
export const wisp_tag_duo = Wisp.Tag.duo;
export const wisp_tag_sym = Wisp.Tag.sym;
export const wisp_tag_fun = Wisp.Tag.fun;
export const wisp_tag_mac = Wisp.Tag.mac;
export const wisp_tag_v32 = Wisp.Tag.v32;
export const wisp_tag_v08 = Wisp.Tag.v08;
export const wisp_tag_pkg = Wisp.Tag.pkg;
export const wisp_tag_ktx = Wisp.Tag.ktx;
export const wisp_tag_run = Wisp.Tag.run;

export const wisp_sys_t: u32 = Wisp.t;
export const wisp_sys_nil: u32 = Wisp.nil;
export const wisp_sys_nah: u32 = Wisp.nah;
export const wisp_sys_zap: u32 = Wisp.zap;
export const wisp_sys_top: u32 = Wisp.top;

export fn wisp_heap_init() ?*Wisp.Heap {
    var orb = std.heap.page_allocator;

    if (orb.create(Wisp.Heap)) |heapptr| {
        if (Wisp.Heap.init(orb, .e0)) |heap| {
            var heap2 = heap;
            Jets.load(&heap2) catch return null;
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

export fn wisp_read(heap: *Wisp.Heap, str: [*:0]const u8) u32 {
    if (Read.read(heap, std.mem.span(str))) |x| {
        return x;
    } else |_| {
        return Wisp.zap;
    }
}

export fn wisp_eval(heap: *Wisp.Heap, exp: u32, max: u32) u32 {
    var run = Step.initRun(exp);
    return Step.evaluate(heap, &run, max) catch Wisp.zap;
}

export fn wisp_run_init(heap: *Wisp.Heap, exp: u32) u32 {
    return heap.new(.run, .{
        .way = Wisp.top,
        .env = Wisp.nil,
        .err = Wisp.nil,
        .val = Wisp.nah,
        .exp = exp,
    }) catch Wisp.zap;
}

export fn wisp_run_eval(
    heap: *Wisp.Heap,
    runptr: u32,
    max: u32,
) u32 {
    var run = heap.row(.run, runptr) catch return Wisp.zap;
    const val = Step.evaluate(heap, &run, max) catch Wisp.zap;
    heap.put(.run, runptr, run) catch return Wisp.zap;
    return val;
}

export fn wisp_eval_step(heap: *Wisp.Heap, runptr: u32) u32 {
    var run = heap.row(.run, runptr) catch return Wisp.zap;

    Step.macroexpand(heap, &run, 10_000) catch return Wisp.zap;
    Step.once(heap, &run) catch return Wisp.zap;

    heap.put(.run, runptr, run) catch return Wisp.zap;

    return if (run.val != Wisp.nah and run.way == Wisp.top)
        Wisp.nil
    else
        Wisp.t;
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

fn TabDat(tag: Wisp.Tag) type {
    const n = std.meta.fields(Wisp.Row(tag)).len;
    var fields: [1 + n]std.builtin.TypeInfo.StructField = undefined;

    fields[0] = Field("n", u32);

    for (std.meta.fields(Wisp.Row(tag))) |field, i| {
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
    run: TabDat(.run),
};

export fn wisp_dat_init(heap: *Wisp.Heap) ?*Dat {
    return heap.orb.create(Dat) catch null;
}

export fn wisp_dat_read(heap: *Wisp.Heap, dat: *Dat) void {
    inline for (Wisp.pointerTags) |tag| {
        const E = std.meta.FieldEnum(Wisp.Row(tag));
        const tab = heap.tab(tag);
        var tagdat = &@field(dat, @tagName(tag));
        tagdat.n = @intCast(u32, tab.list.len);
        inline for (std.meta.fields(Wisp.Row(tag))) |field, i| {
            const slice = tab.list.slice();
            @field(tagdat, field.name) = @ptrToInt(slice.items(@intToEnum(E, i)).ptr);
        }
    }
}

export fn wisp_heap_v08_len(heap: *Wisp.Heap) usize {
    return heap.v08.items.len;
}

export fn wisp_heap_v08_ptr(heap: *Wisp.Heap) [*]u8 {
    return heap.v08.items.ptr;
}

export fn wisp_heap_v32_len(heap: *Wisp.Heap) usize {
    return heap.v32.list.items.len;
}

export fn wisp_heap_v32_ptr(heap: *Wisp.Heap) [*]u32 {
    return heap.v32.list.items.ptr;
}

export fn wisp_alloc(heap: *Wisp.Heap, n: u32) usize {
    const buf = heap.orb.alloc(u8, n) catch return 0;
    return @ptrToInt(buf.ptr);
}

export fn wisp_free(heap: *Wisp.Heap, x: [*:0]u8) void {
    heap.orb.free(std.mem.span(x));
}

export fn wisp_destroy(heap: *Wisp.Heap, x: [*]u8) void {
    heap.orb.destroy(x);
}

export fn wisp_jet_name(x: u32) usize {
    return @ptrToInt(Jets.jets[Wisp.Imm.from(x).idx].txt.ptr);
}

export fn wisp_jet_name_len(x: u32) usize {
    return Jets.jets[Wisp.Imm.from(x).idx].txt.len;
}

test "sanity" {
    try std.testing.expectEqual(0x88000000, wisp_sys_nil);
}
