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

import idom from "./lib/idom.js"
import git from "./lib/git.js"
import cm from "./lib/codemirror.js"

import * as grammar from "./lib/wisplang.js"

class Wisp {
  instance
  api
  heap
  sys

  constructor(instance) {
    this.instance = instance
    this.api = this.instance.exports
    this.sys = this.loadSys()
    this.heap = this.api.wisp_heap_init()
  }

  loadSys() {
    const mem = new DataView(this.api.memory.buffer)
    const u32 = x => mem.getUint32(x, true)

    const sys = x =>
      u32(this.api[`wisp_sys_${x}`].value)

    return {
      t: sys("t"),
      nil: sys("nil"),
      nah: sys("nah"),
      zap: sys("zap"),
      top: sys("top")
    }
  }

  newExt(idx) {
    return this.api.wisp_heap_new_ext(this.heap, idx) >>> 0
  }

  extidx(ext) {
    return this.api.wisp_heap_get_ext_idx(this.heap, ext) >>> 0
  }

  freePin(pin) {
    this.api.wisp_heap_free_pin(this.heap, pin)
  }

  loadString(v08) {
    let ptr = this.api.wisp_heap_get_v08_ptr(this.heap, v08)
    let len = this.api.wisp_heap_get_v08_len(this.heap, v08)
    return this.getString(ptr, len)
  }

  getString(ptr, len) {
    let buffer = new Uint8Array(this.api.memory.buffer, ptr, len)
    let decoder = new TextDecoder
    return decoder.decode(buffer)
  }

  loadVector(v32) {
    let ptr = this.api.wisp_heap_get_v32_ptr(this.heap, v32)
    let len = this.api.wisp_heap_get_v32_len(this.heap, v32)
    return this.getVector(ptr, len)
  }

  getVector(ptr, len) {
    let buffer = new Uint32Array(this.api.memory.buffer, ptr, len)
    return [].slice.call(Array.from(buffer)).map(x => x >>> 0)
  }

  allocString(s) {
    const arr = new TextEncoder().encode(s)
    const buf = this.api.wisp_alloc(this.heap, arr.length + 1)
    const mem = new Uint8Array(this.api.memory.buffer, buf, arr.length + 1)

    mem.set(arr)
    mem[arr.length] = 0

    return buf
  }

  newv08(s) {
    const buf = this.allocString(s)
    const v08 = this.api.wisp_heap_v08_new(this.heap, buf, s.length)
    this.free_n(buf, s.length)
    return v08 >>> 0
  }

  newv32(arr) {
    const buf = this.api.wisp_alloc(this.heap, 4 * arr.length)
    const mem = new Uint32Array(this.api.memory.buffer, buf, arr.length)
    mem.set(arr)
    mem[arr.length] = 0

    const v32 = this.api.wisp_heap_v32_new(this.heap, buf, arr.length)
    this.free_n(buf, 4 * arr.length)

    return v32 >>> 0
  }

  internKeyword(s) {
    const buf = this.allocString(s)
    const x = this.api.wisp_intern_keyword(this.heap, buf, s.length)
    this.free_0(buf)
    return x >>> 0
  }

  free_0(...xs) {
    for (const x of xs) {
      this.api.wisp_free_0(this.heap, x)
    }
  }

  free_n(x, n) {
    this.api.wisp_free_n(this.heap, x, n)
  }

  read(sexp) {
    const buf = this.allocString(sexp)
    const x = this.api.wisp_read(this.heap, buf)
    this.free_0(buf)
    return x >>> 0
  }

  readMany(sexp) {
    const buf = this.allocString(sexp)
    const x = this.api.wisp_read_many(this.heap, buf)
    this.free_0(buf)
    return x >>> 0
  }

  eval(exp) {
    return this.api.wisp_eval(this.heap, exp, 10000) >>> 0
  }
}

const WASI_ESUCCESS = 0
const WASI_STDOUT_FILENO = 1
const WASI_STDERR_FILENO = 2

const CLOCK = {
  REALTIME: 0,
  MONOTONIC: 1,
}

class WASI {
  memory
  buffers

  constructor() {
    this.buffers = {
      [WASI_STDOUT_FILENO]: [],
      [WASI_STDERR_FILENO]: [],
    }
  }

  setMemory(memory) {
    this.memory = memory
  }

  getDataView() {
    return new DataView(this.memory.buffer)
  }

  exports() {
    return {
      proc_exit() {},

      fd_prestat_get() {},
      fd_prestat_dir_name() {},

      fd_write: (fd, iovs, iovsLen, nwritten) => {
        const view = this.getDataView()
        let written = 0

        const buffers = Array.from({ length: iovsLen }, (_, i) => {
          const ptr = iovs + i * 8
          const buf = view.getUint32(ptr, !0)
          const bufLen = view.getUint32(ptr + 4, !0)

          return new Uint8Array(
            this.memory.buffer, buf, bufLen
          )
        })

        // XXX: verify that this handles multiple lines correctly
        for (let iov of buffers) {
          const newline = 10
          let i = 0
          while (true) {
            let newlineIndex = iov.indexOf(newline, i)
            if (newlineIndex > -1) {
              let line = "", decoder = new TextDecoder
              for (let buffer of this.buffers[fd])
                line += decoder.decode(buffer, { stream: true })
              line += decoder.decode(iov.slice(0, newlineIndex))

              if (fd === WASI_STDOUT_FILENO) console.log(line)
              else if (fd === WASI_STDERR_FILENO) console.warn(line)

              this.buffers[fd] = [iov.slice(newlineIndex + 1)]
              i = newlineIndex + 1
            } else {
              break
            }
          }

          this.buffers[fd].push(new Uint8Array(iov.slice(i)))

          written += iov.byteLength
        }


        view.setUint32(nwritten, written, !0)

        return WASI_ESUCCESS
      },

      fd_close() {},
      fd_read() {},

      path_open() {},
      path_rename() {},
      path_create_directory() {},
      path_remove_directory() {},
      path_unlink_file() {},

      fd_filestat_get() {},

      random_get: (buf_ptr, buf_len) => {
        const buffer = new Uint8Array(this.memory.buffer, buf_ptr, buf_len)
        crypto.getRandomValues(buffer)

        return 0
      },

      clock_time_get: (
        clock_id,
        _precision,
        timestamp_out,
      ) => {
        const view = this.getDataView()

        switch (clock_id) {
          case CLOCK.REALTIME:
          case CLOCK.MONOTONIC: {
            const t = BigInt(Date.now()) * BigInt(1e6)
            view.setBigUint64(timestamp_out, t, true)
            break
          }

          default:
            throw new Error("unhandled clock type")
        }

        return 0
      },
    }
  }
}


class WASD {
  wisp

  ext
  elements

  nextExt = 1
  nextElementId = 1

  constructor() {
    this.ext = new Map
    this.elements = new Map
  }

  setWisp(wisp) {
    this.wisp = wisp
  }

  newExt(x) {
    this.ext.set(this.nextExt, x)
    return this.wisp.newExt(this.nextExt++)
  }

  convert(x) {
    console.log("converting", x)
    if (Number.isInteger(x)) {
      return x
    } else if (typeof x === "string") {
      return this.wisp.newv08(x)
    } else if (Array.isArray(x)) {
      return this.wisp.newv32(x.map(x => this.convert(x)))
    } else if (x === null || x === undefined || x === false) {
      return this.wisp.sys.nil
    } else if (x === true) {
      return this.wisp.sys.t
    } else if (x instanceof NodeList || x instanceof HTMLCollection) {
      return this.convert(Array.from(x))
    } else {
      return this.newExt(x)
    }
  }

  convertFromWisp(x) {
    x = x >>> 0
    if (0 === (x & ((1 << 31) >>> 0))) {
      return x
    } else if (x === this.wisp.sys.nil) {
      return null
    } else if (x === this.wisp.sys.t) {
      return true
    } else {
      const tags = {
        v08: 0x1a,
        v32: 0x19,
        ext: 0x1e,
        pin: 0x1f,
        duo: 0x15,
      }
      let tag = (x & ((0b11111 << (32 - 5)) >>> 0)) >>> (32 - 5)
      if (tag === tags.v08) {
        return this.wisp.loadString(x >>> 0)
      } else if (tag === tags.v32) {
        return this.wisp.loadVector(x >>> 0).map(x => this.convertFromWisp(x))
      } else if (tag === tags.ext) {
        let y = this.ext.get(this.wisp.extidx(x))
        if (y === undefined) {
          throw new Error(`unknown foreign object ${x}`)
        }
        return y
      } else if (tag === tags.pin) {
        // assuming this is a callback
        return data => {
          let result = this.wisp.api.wisp_call(
            this.wisp.heap, x, this.wisp.api.wisp_cons(
              this.wisp.heap,
              this.convert(data),
              this.wisp.sys.nil)
          )

          return this.convertFromWisp(result)
        }
      } else if (tag === tags.duo) {
        console.warn("ignoring cons return")
        return null

      } else {
        throw new Error(`unknown pointer type 0x${tag.toString(16)}`)
      }
    }
  }

  exports() {
    return {
      release: idx => {
        console.log("releasing", idx, this.ext.get(idx >>> 0))
        this.ext.delete(idx >>> 0)
      },

      object: (ptr, len) => {
        let v32 = this.wisp.getVector(ptr, len)
        let args = v32.map(x => this.convertFromWisp(x >>> 0))

        let result = {}
        for (let i = 0; i < args.length; i += 2) {
          result[args[i]] = args[i + 1]
        }

        return this.newExt(result)
      },

      globalThis: () => {
        return this.newExt(globalThis)
      },

      call: (id, strptr, strlen, argptr, arglen) => {
        let o = this.ext.get(id)
        let v32 = this.wisp.getVector(argptr, arglen)
        let args = v32.map(x => this.convertFromWisp(x))

        let functionName = this.wisp.getString(strptr, strlen)
        let x = o[functionName].apply(o, args)
        let y = this.convert(x)

        console.log({ functionName, args, id, ext: this.ext, o, y })
        return y
      },

      get: (id, strptr, strlen) => {
        let o = this.ext.get(id)
        let name = this.wisp.getString(strptr, strlen)
        let x = o[name]
        let y = this.convert(x)
        console.info("get", { id, name, o, x, y, ext: this.ext })

        return y
      },

      patch: (elementId, funptr, data) => {
        try {
          let element = this.convertFromWisp(elementId)
          let fun = this.convertFromWisp(funptr)

          // We now run this synchronously, reentrantly.  This only
          // works because we inhibit garbage collection
          // during callbacks.
          idom.patch(element, fun, data)

          return 0
        } catch (e) {
          throw e
        }
      },

      open_start: (tagptr, taglen) => {
        let tag = this.wisp.getString(tagptr, taglen)
        idom.elementOpenStart(tag.toLowerCase())
      },

      open_end: () => {
        idom.elementOpenEnd()
      },

      attr: (
        attrptr, attrlen, valptr, vallen
      ) => {
        let attr = this.wisp.getString(attrptr, attrlen)
        let val = this.wisp.getString(valptr, vallen)
        idom.attr(attr.toLowerCase(), val)
      },

      close: (tagptr, taglen) => {
        let tag = this.wisp.getString(tagptr, taglen)
        idom.elementClose(tag.toLowerCase())
      },

      text: (ptr, len) => {
        let text = this.wisp.getString(ptr, len)
        idom.text(text)
      },

      on_keydown: funptr => {
        let callback = x =>
          this.wisp.api.wisp_call(
            this.wisp.heap, funptr,
            this.wisp.api.wisp_cons(this.wisp.heap, x, this.wisp.sys.nil))

        window.addEventListener(
          "keydown",
          key => {
            let editor = key.target.closest(".cm-editor")
            if (!editor) {
              let truth = x => x ? this.wisp.sys.t : this.wisp.sys.nil
              let result = callback(this.wisp.newv32([
                this.wisp.newv08(key.key),
                truth(key.ctrlKey),
                truth(key.shiftKey),
                truth(key.altKey),
                truth(key.metaKey),
                truth(key.repeat),
              ]))

              if (result >>> 0 !== this.wisp.sys.t) {
                key.preventDefault()
                key.stopImmediatePropagation()
              }
            }
          },
        )
      },

      addWindowEventHandler: (ptr, len, funptr) => {
        let eventName = this.wisp.getString(ptr, len)

        window.addEventListener(eventName, event => {
          let callback = x =>
            this.wisp.api.wisp_call(
              this.wisp.heap, funptr,
              this.wisp.api.wisp_cons(this.wisp.heap, x, this.wisp.sys.nil))

          callback(event.detail)
        })
      },
    }
  }
}

function domCode(root) {
  const data = root.dataset
  if (root.matches(".cursor")) {
    return " "
  } else if (root.matches("#file")) {
    let items = [].slice.call(root.children).map(domCode)
    return `${items.join("\n")}`
  } else if (root.matches(".symbol")) {
    if (data.packageName === "KEYWORD")
      return `:${data.symbolName}`
    else
      return `${data.symbolName}`
  } else if (root.matches(".number")) {
    return root.innerText
  } else if (root.matches(".string")) {
    return `"${root.innerText}"`
  } else if (root.matches(".vector")) {
    let items = [].slice.call(root.children).map(domCode)
    return `[${items.join(" ")}]`
  } else if (root.matches(".list")) {
    let items = [].slice.call(root.children).map(domCode)
    return `(${items.join(" ")})`
  } else if (root.matches("input[type=checkbox]")) {
    if (root.checked) {
      return "done"
    } else {
      return "todo"
    }
  }

  console.error("unknown element", root)
  throw new Error("unknown element")
}

onload = async () => {
  const wasi = new WASI
  const wasd = new WASD

  const { instance } = await WebAssembly.instantiateStreaming(
    fetch("dist/wisp.wasm"), {
      wasi_snapshot_preview1: wasi.exports(),
      dom: wasd.exports(),
    }
  )

  const exports = instance.exports

  wasi.setMemory(exports.memory)

  let ctx = new Wisp(instance)

  wasd.setWisp(ctx)

  if (ctx.api.wisp_start_web(ctx.heap) >>> 0 != ctx.sys.t)
    throw new Error("wisp start web failed")

  function exec(code) {
    const src = ctx.read(`(progn\n${code}\n)`)
    const run = ctx.api.wisp_run_init(ctx.heap, src)
    ctx.api.wisp_run_eval(ctx.heap, run, 4_000_000)
  }

  const basecode = await fetch("./dexp.wisp").then(x => x.text())
  const usercode = await fetch("./user.wisp").then(x => x.text())
  
  exec(basecode)
  exec(usercode)

  const file = localStorage.getItem("wisp-file") || usercode
  const forms = file ? ctx.readMany(file) : ctx.sys.nil
  let packageName = "WISP"
  let functionName = "WISP-BOOT"
  let pkgname = ctx.allocString(packageName)
  let funname = ctx.allocString(functionName)
  let result = ctx.api.wisp_call_package_function(
    ctx.heap,
    pkgname, packageName.length,
    funname, functionName.length,
    forms,
  )

  if (result === ctx.sys.zap)
    throw new Error

  ctx.free_0(pkgname, funname)
}

function startEditor(
  element, doc, symbols, done,
) {
  function onSubmit(view) {
    done(view.state.doc.sliceString(0))
  }

  const view = new cm.view.EditorView({
    parent: element,
    dispatch: tx => view.update([tx]),
    state: makeEditorState(doc),
  })

  view.focus()

  function makeEditorState(doc) {
    return cm.state.EditorState.create({
      doc,
      extensions: [
        cm.view.EditorView.theme({
          ".cm-completionIcon": {
            width: "1.5em",
          },

          ".cm-focused": {
            outline: "none !important",
          },

          "&": {
            fontSize: 16,
          },

          ".cm-scroller, .cm-content, .cm-tooltip.cm-tooltip-autocomplete > ul": {
            fontFamily: "berkeley mono, dm mono, ui-monospace, SFMono-Regular, Menlo, monospace",
            fontWeight: "normal",
          },

          ".cm-gutters": {
              backgroundColor: "#2223 !important",
          },

          ".cm-activeLineGutter": {
              backgroundColor: "#2228 !important"
          },

          ".cm-activeLine": {
              backgroundColor: "#1119 !important"
          },

          ".cm-panels": {
            backgroundColor: "#222 !important",
            zIndex: 2,
            borderBottom: "1px solid #555",
            padding: 5,
          },
        }, {
          dark: true,
        }),
        
        cm.highlight.defaultHighlightStyle.fallback,
        cm.highlight.HighlightStyle.define([{
          tag: cm.highlight.tags.keyword,
          color: "yellow",
        }]).extension,
        
        cm.view.highlightSpecialChars(),
        cm.view.drawSelection(),
        cm.view.dropCursor(),
        cm.view.highlightActiveLine(),
        
        cm.language.indentOnInput(),
        
        cm.matchbrackets.bracketMatching(),
        cm.closebrackets.closeBrackets(),
        cm.autocomplete.autocompletion(),
        
        cm.view.EditorView.lineWrapping,
        
        cm.view.keymap.of([
          {
            key: "Enter",
            run(view) {
              onSubmit(view)
              return true
            }
          },
          cm.commands.indentWithTab,
          ...cm.closebrackets.closeBracketsKeymap,
          ...cm.commands.defaultKeymap,
          ...cm.commands.emacsStyleKeymap,
          ...cm.comment.commentKeymap,
          ...cm.autocomplete.completionKeymap,
        ]),

        wisplang(symbols),
      ],
    })
  }
}

const hang = n => node => {
  let kids = node.getChildren("Form")
  let first = kids[n - 1], last = node.lastChild
  if (first && first.to < last.from) {
    return {
      from: first.to,
      to: last.type.isError ? node.to : last.from
    }
  }
  return null
}

let wispLanguage = cm.language.LRLanguage.define({
  parser: grammar.parser.configure({
    props: [
      cm.language.indentNodeProp.add({
        Defun: cm.language.continuedIndent(),
        Let: cm.language.continuedIndent(),
        Fn: cm.language.continuedIndent(),
      }),
      cm.language.foldNodeProp.add({
        Defun: hang(2),
        Let: hang(1),
        Fn: hang(1),
      }),
      cm.highlight.styleTags({
        "defun let fn run": cm.highlight.tags.keyword,
        Symbol: cm.highlight.tags.variableName,
        Key: cm.highlight.tags.docComment,
        ZB32: cm.highlight.tags.meta,
        Date: cm.highlight.tags.number,
        String: cm.highlight.tags.string,
        Integer: cm.highlight.tags.integer,
        Comment: cm.highlight.tags.lineComment,
        "( )": cm.highlight.tags.paren,
      }),
    ],
  }),

  languageData: {
    commentTokens: { line: ";" },
    indentOnInput: /^\s*\)$/,
    closeBrackets: {
      brackets: ["("],
      before: `");`,
    },
  },
})

let wispCompletion = symbols =>
  wispLanguage.data.of({
    autocomplete: cm.autocomplete.completeFromList(
      symbols.map(x => ({
        label: x,
        type: "function",
      }))
    )
  })

function wisplang(symbols) {
  return new cm.language.LanguageSupport(
    wispLanguage,
    [wispCompletion(symbols)]
  )
}


window.fs = new git.fs("wisp")
window.git = git.git
window.git_http = git.http

window.wisp = {
  startEditor,
  domCode,
}

