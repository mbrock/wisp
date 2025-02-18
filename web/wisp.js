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

import idom from "./lib/idom.js";

export class Wisp {
  instance;
  api;
  heap;
  sys;

  constructor(instance) {
    this.instance = instance;
    this.api = this.instance.exports;
    this.sys = this.loadSys();
    this.heap = this.api.wisp_heap_init();
  }

  loadSys() {
    const mem = new DataView(this.api.memory.buffer);
    const u32 = (x) => mem.getUint32(x, true);

    const sys = (x) => u32(this.api[`wisp_sys_${x}`].value);

    return {
      t: sys("t"),
      nil: sys("nil"),
      nah: sys("nah"),
      zap: sys("zap"),
      top: sys("top"),
    };
  }

  newExt(idx) {
    return this.api.wisp_heap_new_ext(this.heap, idx) >>> 0;
  }

  extidx(ext) {
    return this.api.wisp_heap_get_ext_idx(this.heap, ext) >>> 0;
  }

  freePin(pin) {
    this.api.wisp_heap_free_pin(this.heap, pin);
  }

  loadString(v08) {
    let ptr = this.api.wisp_heap_get_v08_ptr(this.heap, v08);
    let len = this.api.wisp_heap_get_v08_len(this.heap, v08);
    return this.getString(ptr, len);
  }

  getString(ptr, len) {
    let buffer = new Uint8Array(this.api.memory.buffer, ptr, len);
    let decoder = new TextDecoder();
    return decoder.decode(buffer);
  }

  loadVector(v32) {
    let ptr = this.api.wisp_heap_get_v32_ptr(this.heap, v32);
    let len = this.api.wisp_heap_get_v32_len(this.heap, v32);
    return this.getVector(ptr, len);
  }

  getVector(ptr, len) {
    let buffer = new Uint32Array(this.api.memory.buffer, ptr, len);
    return [].slice.call(Array.from(buffer)).map((x) => x >>> 0);
  }

  allocString(s) {
    const arr = new TextEncoder().encode(s);
    const buf = this.api.wisp_alloc(this.heap, arr.length + 1);
    const mem = new Uint8Array(this.api.memory.buffer, buf, arr.length + 1);

    mem.set(arr);
    mem[arr.length] = 0;

    return buf;
  }

  newv08(s) {
    const buf = this.allocString(s);
    const v08 = this.api.wisp_heap_v08_new(this.heap, buf, s.length);
    this.free_n(buf, s.length);
    return v08 >>> 0;
  }

  newv32(arr) {
    const buf =
      arr.length === 0 ? 0 : this.api.wisp_alloc(this.heap, 4 * arr.length);
    const mem = new Uint32Array(this.api.memory.buffer, buf, arr.length);
    mem.set(arr);
    mem[arr.length] = 0;

    const v32 = this.api.wisp_heap_v32_new(this.heap, buf, arr.length);

    if (buf !== 0) {
      this.free_n(buf, 4 * arr.length);
    }

    return v32 >>> 0;
  }

  internKeyword(s) {
    const buf = this.allocString(s);
    const x = this.api.wisp_intern_keyword(this.heap, buf, s.length);
    this.free_0(buf);
    return x >>> 0;
  }

  free_0(...xs) {
    for (const x of xs) {
      this.api.wisp_free_0(this.heap, x);
    }
  }

  free_n(x, n) {
    this.api.wisp_free_n(this.heap, x, n);
  }

  read(sexp) {
    const buf = this.allocString(sexp);
    const x = this.api.wisp_read(this.heap, buf);
    this.free_0(buf);
    return x >>> 0;
  }

  readMany(sexp) {
    const buf = this.allocString(sexp);
    const x = this.api.wisp_read_many(this.heap, buf);
    this.free_0(buf);
    return x >>> 0;
  }

  eval(exp) {
    return this.api.wisp_eval(this.heap, exp, 10000) >>> 0;
  }
}

export class WASD {
  wisp;

  ext;
  elements;

  nextExt = 1;
  nextElementId = 1;

  constructor() {
    this.ext = new Map();
    this.elements = new Map();
  }

  setWisp(wisp) {
    this.wisp = wisp;
  }

  newExt(x) {
    this.ext.set(this.nextExt, x);
    return this.wisp.newExt(this.nextExt++);
  }

  convert(x) {
    if (Number.isInteger(x)) {
      return x;
    } else if (typeof x === "string") {
      return this.wisp.newv08(x);
    } else if (Array.isArray(x)) {
      return this.wisp.newv32(x.map((x) => this.convert(x)));
    } else if (x === null || x === undefined || x === false) {
      return this.wisp.sys.nil;
    } else if (x === true) {
      return this.wisp.sys.t;
    } else if (x instanceof NodeList || x instanceof HTMLCollection) {
      return this.convert(Array.from(x));
    } else {
      return this.newExt(x);
    }
  }

  convertFromWisp(x) {
    x = x >>> 0;
    if (0 === (x & ((1 << 31) >>> 0))) {
      return x;
    } else if (x === this.wisp.sys.nil) {
      return null;
    } else if (x === this.wisp.sys.t) {
      return true;
    } else {
      const tags = {
        v08: 0x1a,
        v32: 0x19,
        ext: 0x1e,
        pin: 0x1f,
        duo: 0x15,
      };
      let tag = (x & ((0b11111 << (32 - 5)) >>> 0)) >>> (32 - 5);
      if (tag === tags.v08) {
        return this.wisp.loadString(x >>> 0);
      } else if (tag === tags.v32) {
        return this.wisp
          .loadVector(x >>> 0)
          .map((x) => this.convertFromWisp(x));
      } else if (tag === tags.ext) {
        let y = this.ext.get(this.wisp.extidx(x));
        if (y === undefined) {
          throw new Error(`unknown foreign object ${x}`);
        }
        return y;
      } else if (tag === tags.pin) {
        // assuming this is a callback
        return (data) => {
          let result = this.wisp.api.wisp_call(
            this.wisp.heap,
            x,
            this.wisp.api.wisp_cons(
              this.wisp.heap,
              this.convert(data),
              this.wisp.sys.nil
            )
          );

          return this.convertFromWisp(result);
        };
      } else if (tag === tags.duo) {
        console.warn("ignoring cons return");
        return null;
      } else {
        throw new Error(`unknown pointer type 0x${tag.toString(16)}`);
      }
    }
  }

  exports() {
    return {
      release: (idx) => {
        console.log("releasing", idx, this.ext.get(idx >>> 0));
        this.ext.delete(idx >>> 0);
      },

      object: (ptr, len) => {
        let v32 = this.wisp.getVector(ptr, len);
        let args = v32.map((x) => this.convertFromWisp(x >>> 0));

        let result = {};
        for (let i = 0; i < args.length; i += 2) {
          result[args[i]] = args[i + 1];
        }

        return this.newExt(result);
      },

      globalThis: () => {
        return this.newExt(globalThis);
      },

      new: (id, argptr, arglen) => {
        let o = this.ext.get(id);
        let v32 = this.wisp.getVector(argptr, arglen);
        let args = v32.map((x) => this.convertFromWisp(x));
        let y = this.convert(new o(...args));
        return y;
      },

      call: (id, strptr, strlen, argptr, arglen) => {
        let o = this.ext.get(id);
        let v32 = this.wisp.getVector(argptr, arglen);
        let args = v32.map((x) => this.convertFromWisp(x));

        let functionName = this.wisp.getString(strptr, strlen);
        let x = o[functionName].apply(o, args);
        let y = this.convert(x);

        return y;
      },

      callFunction: (id, argptr, arglen) => {
        let o = this.ext.get(id);
        let v32 = this.wisp.getVector(argptr, arglen);
        let args = v32.map((x) => this.convertFromWisp(x));

        let x = o.apply(null, args);
        let y = this.convert(x);

        return y;
      },

      get: (id, strptr, strlen) => {
        let o = this.ext.get(id);
        let name = this.wisp.getString(strptr, strlen);
        let x = o[name];
        let y = this.convert(x);

        return y;
      },

      set: (id, strptr, strlen, val) => {
        let o = this.ext.get(id);
        let name = this.wisp.getString(strptr, strlen);
        o[name] = this.convertFromWisp(val);
      },

      patch: (elementId, funptr, data) => {
        try {
          let element = this.convertFromWisp(elementId);
          let fun = this.convertFromWisp(funptr);

          // We now run this synchronously, reentrantly.  This only
          // works because we inhibit garbage collection
          // during callbacks.
          idom.patch(element, fun, data);

          return 0;
        } catch (e) {
          throw e;
        }
      },

      open_start: (tagptr, taglen) => {
        let tag = this.wisp.getString(tagptr, taglen);
        idom.elementOpenStart(tag.toLowerCase());
      },

      open_end: () => {
        idom.elementOpenEnd();
      },

      attr: (attrptr, attrlen, valptr, vallen) => {
        let attr = this.wisp.getString(attrptr, attrlen);
        let val = this.wisp.getString(valptr, vallen);
        idom.attr(attr.toLowerCase(), val);
      },

      close: (tagptr, taglen) => {
        let tag = this.wisp.getString(tagptr, taglen);
        idom.elementClose(tag.toLowerCase());
      },

      text: (ptr, len) => {
        let text = this.wisp.getString(ptr, len);
        idom.text(text);
      },

      on_keydown: (funptr) => {
        let callback = (x) =>
          this.wisp.api.wisp_call(
            this.wisp.heap,
            funptr,
            this.wisp.api.wisp_cons(this.wisp.heap, x, this.wisp.sys.nil)
          );

        window.addEventListener("keydown", (key) => {
          let editor = key.target.closest(".cm-editor");
          if (!editor) {
            let truth = (x) => (x ? this.wisp.sys.t : this.wisp.sys.nil);
            let result = callback(
              this.wisp.newv32([
                this.wisp.newv08(key.key),
                truth(key.ctrlKey),
                truth(key.shiftKey),
                truth(key.altKey),
                truth(key.metaKey),
                truth(key.repeat),
              ])
            );

            if (result >>> 0 !== this.wisp.sys.t) {
              key.preventDefault();
              key.stopImmediatePropagation();
            }
          }
        });
      },

      addWindowEventHandler: (ptr, len, funptr) => {
        let eventName = this.wisp.getString(ptr, len);

        window.addEventListener(eventName, (event) => {
          let callback = (x) =>
            this.wisp.api.wisp_call(
              this.wisp.heap,
              funptr,
              this.wisp.api.wisp_cons(this.wisp.heap, x, this.wisp.sys.nil)
            );

          callback(event.detail);
        });
      },
    };
  }
}

export function domCode(root) {
  const data = root.dataset;
  if (root.matches(".cursor")) {
    return " ";
  } else if (root.matches("main")) {
    let items = [].slice.call(root.children).map(domCode);
    return `${items.join("\n")}`;
  } else if (root.matches(".symbol")) {
    if (data.packageName === "KEYWORD") return `:${data.symbolName}`;
    else return `${data.symbolName}`;
  } else if (root.matches(".number")) {
    return root.innerText;
  } else if (root.matches(".string")) {
    return `"${root.innerText}"`;
  } else if (root.matches(".vector")) {
    let items = [].slice.call(root.children).map(domCode);
    return `[${items.join(" ")}]`;
  } else if (root.matches(".list")) {
    let items = [].slice.call(root.children).map(domCode);
    return `(${items.join(" ")})`;
  } else if (root.matches("input[type=checkbox]")) {
    if (root.checked) {
      return "done";
    } else {
      return "todo";
    }
  }

  console.error("unknown element", root);
  throw new Error("unknown element");
}
