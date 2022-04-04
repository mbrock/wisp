import * as DOM from "incremental-dom"
import { Wisp } from "./wisp"

type U32 = number

// I couldn't resist the name.  It's like WASI but for the DOM.
//
// We use the "Incremental DOM" library for a simple but efficient way
// to update DOM nodes which we can very easily bind to Wisp.
//
export class WASD {
  wisp: Wisp

  ext: Map<U32, any>
  elements: Map<U32, HTMLElement>

  nextExt = 1
  nextElementId = 1

  constructor() {
    this.ext = new Map
    this.elements = new Map
  }

  setWisp(wisp: Wisp) {
    this.wisp = wisp
  }

  newExt(x: any): U32 {
    this.ext.set(this.nextExt, x)
    return this.wisp.newExt(this.nextExt++)
  }

  convert(x: any): U32 {
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

  convertFromWisp(x: U32): any {
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
        return (data: any) => {
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
      release: (idx: number) => {
        console.log("releasing", idx, this.ext.get(idx >>> 0))
        this.ext.delete(idx >>> 0)
      },

      object: (ptr: number, len: number) => {
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

      call: (id: U32, strptr: U32, strlen: U32, argptr: U32, arglen: U32) => {
        let o = this.ext.get(id)
        let v32 = this.wisp.getVector(argptr, arglen)
        let args = v32.map(x => this.convertFromWisp(x))

        let functionName = this.wisp.getString(strptr, strlen)
        let x = (o[functionName] as Function).apply(o, args)
        let y = this.convert(x)

        console.log({ functionName, args, id, ext: this.ext, o, y })
        return y
      },

      get: (id: U32, strptr: U32, strlen: U32) => {
        let o = this.ext.get(id)
        let name = this.wisp.getString(strptr, strlen)
        let x = o[name]
        let y = this.convert(x)
        console.info("get", { id, name, o, x, y, ext: this.ext })

        return y
      },

      patch: (elementId: U32, funptr: U32, data: U32) => {
        try {
          let element = this.convertFromWisp(elementId)
          let fun = this.convertFromWisp(funptr)

          // We now run this synchronously, reentrantly.  This only
          // works because we inhibit garbage collection
          // during callbacks.
          DOM.patch(element, fun, data)

          return 0
        } catch (e) {
          throw e
        }
      },

      open_start: (tagptr: U32, taglen: U32) => {
        let tag = this.wisp.getString(tagptr, taglen)
        DOM.elementOpenStart(tag.toLowerCase())
      },

      open_end: () => {
        DOM.elementOpenEnd()
      },

      attr: (
        attrptr: U32, attrlen: U32, valptr: U32, vallen: U32
      ) => {
        let attr = this.wisp.getString(attrptr, attrlen)
        let val = this.wisp.getString(valptr, vallen)
        DOM.attr(attr.toLowerCase(), val)
      },

      // attr_callback: (
      //   attrptr: U32, attrlen: U32, callbackId: U32
      // ) => {
        // let attr = this.wisp.getString(attrptr, attrlen)
        // let callback = this.callbacks.get(callbackId)
        // DOM.attr(attr.toLowerCase(), callback)
      // },

      close: (tagptr: U32, taglen: U32) => {
        let tag = this.wisp.getString(tagptr, taglen)
        DOM.elementClose(tag.toLowerCase())
      },

      text: (ptr: U32, len: U32) => {
        let text = this.wisp.getString(ptr, len)
        DOM.text(text)
      },

      on_keydown: (funptr: U32) => {
        let callback = (x: U32) =>
          this.wisp.api.wisp_call(
            this.wisp.heap, funptr,
            this.wisp.api.wisp_cons(this.wisp.heap, x, this.wisp.sys.nil))

        window.addEventListener(
          "keydown",
          key => {
            let editor = (key.target as HTMLElement).closest(".cm-editor")
            if (!editor) {
              let truth = (x: any) => x ? this.wisp.sys.t : this.wisp.sys.nil
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

      addWindowEventHandler: (ptr: U32, len: U32, funptr: U32) => {
        let eventName = this.wisp.getString(ptr, len)

        window.addEventListener(eventName, (event: CustomEvent) => {
          let callback = (x: U32) =>
            this.wisp.api.wisp_call(
              this.wisp.heap, funptr,
              this.wisp.api.wisp_cons(this.wisp.heap, x, this.wisp.sys.nil))

          callback(event.detail)
        })
      },
    }
  }
}

export function domCode(root: HTMLElement): string {
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
    if ((root as HTMLInputElement).checked) {
      return "done"
    } else {
      return "todo"
    }
  }

  console.error("unknown element", root)
  throw new Error("unknown element")
}

function evalDexp(wisp: Wisp, e: HTMLElement) {
  window.dispatchEvent(
    new CustomEvent("wisp-eval", {
      detail: wisp.read(
        domCode(e)
      )
    })
  )
}
