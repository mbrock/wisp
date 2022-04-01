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
    extern "dom" fn globalThis() u32;

    extern "dom" fn call(
        object: u32,
        prop_ptr: [*]const u8,
        prop_len: u32,
        args_ptr: [*]const u32,
        args_len: usize,
    ) u32;

    extern "dom" fn get(
        object: u32,
        prop_ptr: [*]const u8,
        prop_len: u32,
    ) u32;

    extern "dom" fn query_selector(
        ptr: [*]const u8,
        len: usize,
    ) u32;

    extern "dom" fn removeChildren(node: u32) void;

    extern "dom" fn on_keydown(
        callback: u32,
    ) void;

    extern "dom" fn addWindowEventHandler(
        ptr: [*]const u8,
        len: usize,
        callback: u32,
    ) void;

    extern "dom" fn prompt(
        ptr: [*]const u8,
        len: usize,
    ) u32;

    extern "dom" fn step(
        element: u32,
        direction: u32,
        ctrl: u32,
        shift: u32,
        alt: u32,
    ) void;
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
};

pub fn @"QUERY-SELECTOR"(step: *Step, selector: u32) anyerror!void {
    const str = try step.heap.v08slice(selector);
    const id = DOM.query_selector(str.ptr, str.len);
    step.give(.val, id);
}

pub fn @"DOM-REMOVE-CHILDREN!"(step: *Step, node: u32) anyerror!void {
    DOM.removeChildren(node);
    step.give(.val, node);
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

pub fn @"DOM-ON-WINDOW-EVENT!"(
    step: *Step,
    name: u32,
    callback: u32,
) anyerror!void {
    const text = try step.heap.v08slice(name);
    DOM.addWindowEventHandler(text.ptr, text.len, callback);
    step.give(.val, nil);
}

pub fn @"DOM-CURSOR-STEP!"(
    step: *Step,
    cursor: u32,
    direction: u32,
    ctrl: u32,
    shift: u32,
    alt: u32,
) anyerror!void {
    DOM.step(cursor, direction, ctrl, shift, alt);
    step.give(.val, nil);
}

pub fn @"JS-GLOBAL-THIS"(step: *Step) anyerror!void {
    step.give(.val, DOM.globalThis());
}

pub fn @"JS-CALL"(
    step: *Step,
    ext: u32,
    str: u32,
    arg: []u32,
) anyerror!void {
    const v08 = try step.heap.v08slice(str);
    const x = DOM.call(
        try step.heap.get(.ext, .idx, ext),
        v08.ptr,
        v08.len,
        arg.ptr,
        arg.len,
    );

    step.give(.val, x);
}

pub fn @"JS-GET"(
    step: *Step,
    ext: u32,
    str: u32,
) anyerror!void {
    const v08 = try step.heap.v08slice(str);
    const x = DOM.get(
        try step.heap.get(.ext, .idx, ext),
        v08.ptr,
        v08.len,
    );

    step.give(.val, x);
}
