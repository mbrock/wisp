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

pub const log_level = std.log.Level.warn;

const Wisp = @import("./wisp.zig");
const Read = @import("./sexp-read.zig");
const Sexp = @import("./sexp.zig");
const Step = @import("./step.zig");
const Jets = @import("./jets.zig");
const Keys = @import("./keys.zig");
const Tape = @import("./tape.zig");

pub const wisp_browser: bool = true;

pub fn main() void {}

export fn _initialize() void {}

pub const wisp_tag_int = Wisp.Tag.int;
pub const wisp_tag_sys = Wisp.Tag.sys;
pub const wisp_tag_chr = Wisp.Tag.chr;
pub const wisp_tag_jet = Wisp.Tag.jet;
pub const wisp_tag_duo = Wisp.Tag.duo;
pub const wisp_tag_sym = Wisp.Tag.sym;
pub const wisp_tag_fun = Wisp.Tag.fun;
pub const wisp_tag_mac = Wisp.Tag.mac;
pub const wisp_tag_v32 = Wisp.Tag.v32;
pub const wisp_tag_v08 = Wisp.Tag.v08;
pub const wisp_tag_pkg = Wisp.Tag.pkg;
pub const wisp_tag_run = Wisp.Tag.run;
pub const wisp_tag_ktx = Wisp.Tag.ktx;

export const wisp_sys_t: u32 = Wisp.t;
export const wisp_sys_nil: u32 = Wisp.nil;
export const wisp_sys_nah: u32 = Wisp.nah;
export const wisp_sys_zap: u32 = Wisp.zap;
export const wisp_sys_top: u32 = Wisp.top;

const GPA = std.heap.GeneralPurposeAllocator(.{});
var gpa = GPA{};
var orb = gpa.allocator();

fn heap_init() !*Wisp.Heap {
    const heap = try orb.create(Wisp.Heap);
    heap.* = try Wisp.Heap.fromEmbeddedCore(orb);
    try Jets.load(heap);
    return heap;
}

export fn wisp_heap_init() ?*Wisp.Heap {
    return heap_init() catch null;
}

export fn wisp_read(heap: *Wisp.Heap, str: [*:0]const u8) u32 {
    return Read.read(heap, std.mem.span(str)) catch Wisp.zap;
}

export fn wisp_read_many(heap: *Wisp.Heap, str: [*:0]const u8) u32 {
    var list = Read.readMany(heap, std.mem.span(str)) catch return Wisp.zap;
    defer list.deinit(heap.orb);
    return Wisp.list(heap, list.items) catch Wisp.zap;
}

export fn wisp_eval(heap: *Wisp.Heap, exp: u32, max: u32) u32 {
    var run = Step.initRun(exp);
    if (Step.evaluate(heap, &run, max)) |result| {
        return result;
    } else |e| {
        var stderr_buf: [4096]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
        const stderr = &stderr_writer.interface;
        stderr.print(";; error {any}\n", .{e}) catch return Wisp.zap;
        stderr.flush() catch return Wisp.zap;
        Sexp.warn("err", heap, run.err) catch return Wisp.zap;
        if (run.exp == Wisp.nah)
            Sexp.warn("val", heap, run.val) catch return Wisp.zap
        else
            Sexp.warn("exp", heap, run.exp) catch return Wisp.zap;

        Sexp.warn("ktx", heap, run.way) catch return Wisp.zap;
        return Wisp.zap;
    }
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

fn run_eval(
    heap: *Wisp.Heap,
    runptr: u32,
    max: u32,
) !u32 {
    var run = try heap.row(.run, runptr);
    const val = try Step.evaluate(heap, &run, max);
    try heap.put(.run, runptr, run);
    return val;
}

export fn wisp_run_eval(
    heap: *Wisp.Heap,
    runptr: u32,
    max: u32,
) u32 {
    return run_eval(heap, runptr, max) catch Wisp.zap;
}

const StepMode = enum(u32) {
    into = 0,
    over = 1,
    out = 2,
};

fn eval_step(heap: *Wisp.Heap, runptr: u32, mode: StepMode) !u32 {
    var run = try heap.row(.run, runptr);

    switch (mode) {
        .into => try Step.once(heap, &run),
        .over => try Step.stepOver(heap, &run, 10_000),
        .out => try Step.stepOut(heap, &run, 10_000),
    }

    try heap.put(.run, runptr, run);

    return if (run.val != Wisp.nah and run.way == Wisp.top)
        Wisp.nil
    else
        Wisp.t;
}

export fn wisp_eval_step(heap: *Wisp.Heap, runptr: u32, mode: StepMode) u32 {
    return eval_step(heap, runptr, mode) catch Wisp.zap;
}

fn run_restart(heap: *Wisp.Heap, run: u32, exp: u32) !u32 {
    try heap.set(.run, .exp, run, exp);
    try heap.set(.run, .val, run, Wisp.nah);
    try heap.set(.run, .err, run, Wisp.nil);
    return Wisp.nil;
}

export fn wisp_run_restart(heap: *Wisp.Heap, run: u32, exp: u32) u32 {
    return run_restart(heap, run, exp) catch Wisp.zap;
}

fn Field(comptime name: [:0]const u8, t: type) std.builtin.Type.StructField {
    return .{
        .name = name,
        .type = t,
        .default_value_ptr = null,
        .is_comptime = false,
        .alignment = 0,
    };
}

fn TabDat(tag: Wisp.Tag) type {
    const n = std.meta.fields(Wisp.Row(tag)).len;
    var fields: [1 + n]std.builtin.Type.StructField = undefined;

    fields[0] = Field("n", u32);

    for (std.meta.fields(Wisp.Row(tag)), 0..) |field, i| {
        fields[1 + i] = Field(field.name, usize);
    }

    const t = @Type(std.builtin.Type{
        .@"struct" = .{
            .layout = .@"packed",
            .fields = &fields,
            .decls = &[_]std.builtin.Type.Declaration{},
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
    ext: TabDat(.ext),
};

export fn wisp_dat_init(heap: *Wisp.Heap) ?*Dat {
    return heap.orb.create(Dat) catch null;
}

export fn wisp_dat_read(heap: *Wisp.Heap, dat: *Dat) void {
    inline for (Wisp.pointerTags) |tag| {
        const E = std.meta.FieldEnum(Wisp.Row(tag));
        const tab = heap.tab(tag);
        var tagdat = &@field(dat, @tagName(tag));
        tagdat.n = @as(u32, @intCast(tab.list.len));
        inline for (std.meta.fields(Wisp.Row(tag)), 0..) |field, i| {
            const slice = tab.list.slice();
            @field(tagdat, field.name) = @intFromPtr(slice.items(@as(E, @enumFromInt(i))).ptr);
        }
    }
}

const ColumnLoadParams = struct {
    heap: *Wisp.Heap,
    col: usize,
    len: usize,
};

fn loadColumn(
    comptime tag: Wisp.Tag,
    params: ColumnLoadParams,
) !?[*]u8 {
    var tab = &@field(params.heap.vat, @tagName(tag));

    if (params.len > 0) {
        try tab.list.ensureTotalCapacity(
            params.heap.orb,
            params.len,
        );
    }

    tab.list.shrinkRetainingCapacity(params.len);

    return tab.list.slice().ptrs[params.col];
}

export fn wisp_heap_load_v08(
    heap: *Wisp.Heap,
    len: usize,
) ?[*]u8 {
    heap.v08.resize(heap.orb, len) catch return null;
    return heap.v08.items.ptr;
}

export fn wisp_heap_load_v32(
    heap: *Wisp.Heap,
    len: usize,
) ?[*]u32 {
    heap.v32.list.resize(heap.orb, len * 4) catch return null;
    return heap.v32.list.items.ptr;
}

export fn wisp_heap_load_tab_col(
    heap: *Wisp.Heap,
    tag: usize,
    col: usize,
    len: usize,
) ?[*]u8 {
    const params = ColumnLoadParams{
        .heap = heap,
        .col = col,
        .len = len,
    };

    return switch (@as(Wisp.Tag, @enumFromInt(tag))) {
        .duo => loadColumn(.duo, params),
        .sym => loadColumn(.sym, params),
        .fun => loadColumn(.fun, params),
        .mac => loadColumn(.mac, params),
        .v32 => loadColumn(.v32, params),
        .v08 => loadColumn(.v08, params),
        .pkg => loadColumn(.pkg, params),
        .ktx => loadColumn(.ktx, params),
        .run => loadColumn(.run, params),
        .ext => loadColumn(.ext, params),

        .int, .sys, .chr, .jet, .pin => null,
    } catch null;
}

export fn wisp_heap_new_ext(heap: *Wisp.Heap, idx: u32) u32 {
    return heap.new(.ext, .{ .idx = idx, .val = Wisp.nil }) catch Wisp.zap;
}

export fn wisp_heap_v08_new(heap: *Wisp.Heap, x: [*]u8, n: usize) u32 {
    return heap.newv08(x[0..n]) catch Wisp.zap;
}

export fn wisp_heap_v32_new(heap: *Wisp.Heap, x: [*]u32, n: usize) u32 {
    return heap.newv32(x[0..n]) catch Wisp.zap;
}

export fn wisp_heap_get_ext_idx(heap: *Wisp.Heap, ext: u32) u32 {
    return heap.get(.ext, .idx, ext) catch Wisp.zap;
}

export fn wisp_heap_free_pin(heap: *Wisp.Heap, pin: u32) void {
    heap.releasePin(pin);
}

export fn wisp_heap_get_v08_ptr(heap: *Wisp.Heap, v08: u32) ?[*]const u8 {
    return (heap.v08slice(v08) catch return null).ptr;
}

export fn wisp_heap_get_v08_len(heap: *Wisp.Heap, v08: u32) usize {
    return (heap.v08slice(v08) catch return Wisp.zap).len;
}

export fn wisp_heap_get_v32_ptr(heap: *Wisp.Heap, v32: u32) ?[*]const u32 {
    return (heap.v32slice(v32) catch return null).ptr;
}

export fn wisp_heap_get_v32_len(heap: *Wisp.Heap, v32: u32) usize {
    return (heap.v32slice(v32) catch return Wisp.zap).len;
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
    return @intFromPtr(buf.ptr);
}

export fn wisp_free_0(heap: *Wisp.Heap, x: [*:0]u8) void {
    heap.orb.free(std.mem.span(x));
}

export fn wisp_free_n(heap: *Wisp.Heap, x: [*]u8, n: usize) void {
    heap.orb.free(x[0..n]);
}

export fn wisp_jet_name(x: u32) usize {
    return @intFromPtr(Jets.jets[Wisp.Imm.from(x).idx].txt.ptr);
}

export fn wisp_jet_name_len(x: u32) usize {
    return Jets.jets[Wisp.Imm.from(x).idx].txt.len;
}

export fn wisp_genkey(heap: *Wisp.Heap) u32 {
    return heap.genkey() catch Wisp.zap;
}

export fn wisp_tape_save(
    heap: *Wisp.Heap,
    filename: [*:0]const u8,
) u32 {
    Tape.save(
        heap,
        std.mem.span(filename),
    ) catch return Wisp.zap;

    return Wisp.nil;
}

export fn wisp_call(
    heap: *Wisp.Heap,
    callee: u32,
    argptr: u32,
) u32 {
    const was_inhibited = heap.inhibit_gc;
    defer heap.inhibit_gc = was_inhibited;
    heap.inhibit_gc = true;

    const pinidx = Wisp.Imm.from(callee).idx;
    const funptr = heap.pins.get(pinidx) orelse return Wisp.zap;

    var run = Step.initRun(Wisp.nil);
    var tmp = std.heap.stackFallback(4096, heap.orb);
    var step = Step{ .heap = heap, .run = &run, .tmp = tmp.get() };

    if (step.call(funptr, argptr, false)) {} else |e| {
        var stderr_buf: [4096]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
        const stderr = &stderr_writer.interface;
        stderr.print(
            ";; couldn't call {any} {any}\n",
            .{ funptr, e },
        ) catch return Wisp.zap;
        stderr.flush() catch return Wisp.zap;
        Sexp.warn("err", heap, run.err) catch return Wisp.zap;
        return Wisp.zap;
    }

    if (Step.evaluate(heap, &run, 0)) |result| {
        return result;
    } else |e| {
        var stderr_buf: [4096]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
        const stderr = &stderr_writer.interface;
        stderr.print(";; error {any}\n", .{e}) catch return Wisp.zap;
        stderr.flush() catch return Wisp.zap;
        Sexp.warn("err", heap, run.err) catch return Wisp.zap;
        if (run.exp == Wisp.nah)
            Sexp.warn("val", heap, run.val) catch return Wisp.zap
        else
            Sexp.warn("exp", heap, run.exp) catch return Wisp.zap;

        Sexp.warn("ktx", heap, run.way) catch return Wisp.zap;
        return Wisp.zap;
    }
}

export fn wisp_cons(heap: *Wisp.Heap, car: u32, cdr: u32) u32 {
    return heap.cons(car, cdr) catch Wisp.zap;
}

export fn wisp_call_package_function(
    heap: *Wisp.Heap,
    pkgname_ptr: [*]const u8,
    pkgname_len: usize,
    funname_ptr: [*]const u8,
    funname_len: usize,
    data: u32,
) u32 {
    const pkgname = pkgname_ptr[0..pkgname_len];
    const funname = funname_ptr[0..funname_len];

    if (heap.pkgmap.get(pkgname)) |pkg| {
        const sym = heap.intern(funname, pkg) catch return Wisp.zap;

        const quote = Wisp.list(
            heap,
            [_]u32{ heap.kwd.QUOTE, data },
        ) catch return Wisp.zap;

        const exp = Wisp.list(
            heap,
            [_]u32{ sym, quote },
        ) catch return Wisp.zap;

        const was_inhibited = heap.inhibit_gc;
        defer heap.inhibit_gc = was_inhibited;
        heap.inhibit_gc = true;
        return wisp_eval(heap, exp, 0);
    } else {
        return Wisp.nil;
    }
}

export fn wisp_intern_keyword(
    heap: *Wisp.Heap,
    ptr: [*]const u8,
    len: usize,
) u32 {
    return heap.intern(
        ptr[0..len],
        heap.keywordPackage,
    ) catch Wisp.zap;
}

test "sanity" {
    try std.testing.expectEqual(0x88000000, wisp_sys_nil);
}
