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

const DOM = domInterface();

fn domInterface() type {
    if (@import("builtin").os.tag == .wasi) {
        return realDomInterface();
    } else {
        return fakeDomInterface();
    }
}

fn realDomInterface() type {
    return struct {
        extern "dom" fn make_callback(
            pkgname_ptr: [*]const u8,
            pkgname_len: u32,
            funname_ptr: [*]const u8,
            funname_len: u32,
        ) u32;

        extern "dom" fn query_selector(ptr: [*]const u8, len: u32) u32;
        extern "dom" fn patch(element: u32, callback: u32, data: u32) u32;
        extern "dom" fn open_start(tagptr: [*]const u8, taglen: u32) void;
        extern "dom" fn open_end() void;
        extern "dom" fn close(tagptr: [*]const u8, taglen: u32) void;
        extern "dom" fn text(ptr: [*]const u8, len: u32) void;
        extern "dom" fn attr(
            attrptr: [*]const u8,
            attrlen: u32,
            valptr: [*]const u8,
            vallen: u32,
        ) void;
    };
}

fn fakeDomInterface() type {
    return struct {
        fn make_callback(
            pkgname_ptr: [*]const u8,
            pkgname_len: u32,
            funname_ptr: [*]const u8,
            funname_len: u32,
        ) u32 {
            _ = pkgname_ptr;
            _ = pkgname_len;
            _ = funname_ptr;
            _ = funname_len;
            return 0;
        }

        fn query_selector(ptr: [*]const u8, len: u32) u32 {
            _ = ptr;
            _ = len;
            return 0;
        }

        fn patch(element: u32, callback: u32, data: u32) u32 {
            _ = element;
            _ = callback;
            _ = data;
            return 1;
        }

        fn open_start(tagptr: [*]const u8, taglen: u32) void {
            _ = tagptr;
            _ = taglen;
        }

        fn open_end() void {}

        fn close(tagptr: [*]const u8, taglen: u32) void {
            _ = tagptr;
            _ = taglen;
        }

        fn text(ptr: [*]const u8, len: u32) void {
            _ = ptr;
            _ = len;
        }

        fn attr(
            attrptr: [*]const u8,
            attrlen: u32,
            valptr: [*]const u8,
            vallen: u32,
        ) void {
            _ = attrptr;
            _ = attrlen;
            _ = valptr;
            _ = vallen;
        }
    };
}

pub fn @"DOM-MAKE-CALLBACK"(
    step: *Step,
    pkg: u32,
    fun: u32,
) anyerror!void {
    const pkgstr = try step.heap.v08slice(pkg);
    const funstr = try step.heap.v08slice(fun);
    const id = DOM.make_callback(
        pkgstr.ptr,
        @intCast(u32, pkgstr.len),
        funstr.ptr,
        @intCast(u32, funstr.len),
    );

    step.give(.val, id);
}

pub fn @"QUERY-SELECTOR"(step: *Step, selector: u32) anyerror!void {
    const str = try step.heap.v08slice(selector);
    const id = DOM.query_selector(str.ptr, @intCast(u32, str.len));
    step.give(.val, id);
}

pub fn @"DOM-PATCH!"(
    step: *Step,
    element: u32,
    callback: u32,
    data: u32,
) anyerror!void {
    if (DOM.patch(element, callback, data) == 0) {
        step.give(.val, nil);
    } else {
        return error.DomPatchError;
    }
}

pub fn @"DOM-OPEN-START!"(
    step: *Step,
    tag: u32,
) anyerror!void {
    const str = try step.heap.v08slice(tag);
    _ = DOM.open_start(str.ptr, @intCast(u32, str.len));
    step.give(.val, nil);
}

pub fn @"DOM-OPEN-END!"(
    step: *Step,
) anyerror!void {
    DOM.open_end();
    step.give(.val, nil);
}

pub fn @"DOM-CLOSE!"(
    step: *Step,
    tag: u32,
) anyerror!void {
    const str = try step.heap.v08slice(tag);
    DOM.close(str.ptr, @intCast(u32, str.len));
    step.give(.val, nil);
}

pub fn @"DOM-ATTR!"(
    step: *Step,
    attr: u32,
    val: u32,
) anyerror!void {
    const attrstr = try step.heap.v08slice(attr);
    const valstr = try step.heap.v08slice(val);
    DOM.attr(
        attrstr.ptr,
        @intCast(u32, attrstr.len),
        valstr.ptr,
        @intCast(u32, valstr.len),
    );
    step.give(.val, nil);
}

pub fn @"DOM-TEXT!"(
    step: *Step,
    text: u32,
) anyerror!void {
    const str = try step.heap.v08slice(text);
    DOM.text(str.ptr, @intCast(u32, str.len));
    step.give(.val, nil);
}
