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
const Jets = @import("./jets.zig");
const Step = @import("./step.zig");

const Heap = Wisp.Heap;

const nil = Wisp.nil;

const DOM = struct {
    extern "dom" fn query_selector(ptr: [*]const u8, len: usize) u32;
    extern "dom" fn make_callback(
        pkgname_ptr: [*]const u8,
        pkgname_len: usize,
        funname_ptr: [*]const u8,
        funname_len: usize,
    ) u32;
    extern "dom" fn on_keydown(
        callback: u32,
    ) void;
    extern "dom" fn prompt(ptr: [*]const u8, len: usize) u32;
};

const IDOM = struct {
    extern "dom" fn patch(element: u32, callback: u32, data: u32) u32;
    extern "dom" fn open_start(tagptr: [*]const u8, taglen: usize) void;
    extern "dom" fn open_end() void;
    extern "dom" fn close(tagptr: [*]const u8, taglen: usize) void;
    extern "dom" fn text(ptr: [*]const u8, len: usize) void;
    extern "dom" fn attr(
        attrptr: [*]const u8,
        attrlen: usize,
        valptr: [*]const u8,
        vallen: usize,
    ) void;
    extern "dom" fn attr_callback(
        attrptr: [*]const u8,
        attrlen: usize,
        callback: usize,
    ) void;
};

pub fn @"DOM-MAKE-CALLBACK"(
    step: *Step,
    pkg: u32,
    fun: u32,
) anyerror!void {
    const pkgstr = try step.heap.v08slice(pkg);
    const funstr = try step.heap.v08slice(fun);
    const id = DOM.make_callback(
        pkgstr.ptr,
        pkgstr.len,
        funstr.ptr,
        funstr.len,
    );

    step.give(.val, id);
}

pub fn @"QUERY-SELECTOR"(step: *Step, selector: u32) anyerror!void {
    const str = try step.heap.v08slice(selector);
    const id = DOM.query_selector(str.ptr, str.len);
    step.give(.val, id);
}

pub fn @"IDOM-PATCH!"(
    step: *Step,
    element: u32,
    callback: u32,
    data: u32,
) anyerror!void {
    if (IDOM.patch(element, callback, data) == 0) {
        step.give(.val, nil);
    } else {
        return error.DomPatchError;
    }
}

pub fn @"IDOM-OPEN-START!"(
    step: *Step,
    tag: u32,
) anyerror!void {
    const str = try step.heap.v08slice(tag);
    _ = IDOM.open_start(str.ptr, str.len);
    step.give(.val, nil);
}

pub fn @"IDOM-OPEN-END!"(
    step: *Step,
) anyerror!void {
    IDOM.open_end();
    step.give(.val, nil);
}

pub fn @"IDOM-CLOSE!"(
    step: *Step,
    tag: u32,
) anyerror!void {
    const str = try step.heap.v08slice(tag);
    IDOM.close(str.ptr, str.len);
    step.give(.val, nil);
}

pub fn @"IDOM-ATTR!"(
    step: *Step,
    attr: u32,
    val: u32,
) anyerror!void {
    const attrstr = try step.heap.v08slice(attr);
    const valstr = try step.heap.v08slice(val);
    IDOM.attr(
        attrstr.ptr,
        attrstr.len,
        valstr.ptr,
        valstr.len,
    );
    step.give(.val, nil);
}

pub fn @"IDOM-ATTR-CALLBACK!"(
    step: *Step,
    attr: u32,
    val: u32,
) anyerror!void {
    const attrstr = try step.heap.v08slice(attr);
    IDOM.attr_callback(
        attrstr.ptr,
        attrstr.len,
        val,
    );
    step.give(.val, nil);
}

pub fn @"IDOM-TEXT!"(
    step: *Step,
    text: u32,
) anyerror!void {
    const str = try step.heap.v08slice(text);
    IDOM.text(str.ptr, str.len);
    step.give(.val, nil);
}

pub fn @"DOM-ON-KEYDOWN!"(
    step: *Step,
    callback: u32,
) anyerror!void {
    DOM.on_keydown(callback);
    step.give(.val, nil);
}

pub fn @"DOM-PROMPT"(step: *Step, v08: u32) anyerror!void {
    const text = try step.heap.v08slice(v08);
    step.give(.val, DOM.prompt(text.ptr, text.len));
}
