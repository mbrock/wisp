import * as DOM from "incremental-dom"
import { Wisp } from "./wisp"
import { startEditor } from "./edit"

type U32 = number

export interface Callback {
  packageName: string,
  functionName: string,
}

// I couldn't resist the name.  It's like WASI but for the DOM.
//
// We use the "Incremental DOM" library for a simple but efficient way
// to update DOM nodes which we can very easily bind to Wisp.
//
export class WASD {
  wisp: Wisp
  callbackOperation: (cb: Callback, data: U32) => U32

  elements: Map<U32, HTMLElement>
  callbacks: Map<U32, (data: U32) => U32>

  nextElementId = 1
  nextCallbackId = 1

  constructor() {
    this.elements = new Map
    this.callbacks = new Map
  }

  setWisp(wisp: Wisp) {
    this.wisp = wisp
  }

  setCallbackOperation(
    operation: (cb: Callback, data: U32) => U32
  ) {
    this.callbackOperation = operation
  }

  exports() {
    return {
      make_callback: (
        pkgptr: U32, pkglen: U32,
        funptr: U32, funlen: U32,
      ) => {
        const packageName = this.wisp.getString(pkgptr, pkglen)
        const functionName = this.wisp.getString(funptr, funlen)

        this.callbacks.set(this.nextCallbackId, (data: U32) =>
          this.callbackOperation({ packageName, functionName }, data)
        )

        return this.nextCallbackId++
      },

      query_selector: (selectorPointer: U32, selectorLength: U32) => {
        let selector = this.wisp.getString(selectorPointer, selectorLength)
        let element = document.querySelector(selector)
        if (element !== null) {
          this.elements.set(this.nextElementId, element as HTMLElement)
          return this.nextElementId++
        } else {
          return 0
        }
      },

      removeChildren: (elementId: U32) => {
        let element = this.elements.get(elementId)
        if (element) {
          element.innerHTML = ""
        }
      },

      patch: (elementId: U32, callbackId: U32, data: U32) => {
        try {
          let element = this.elements.get(elementId)
          let callback = this.callbacks.get(callbackId)

          // We now run this synchronously, reentrantly.  This only
          // works because we inhibit garbage collection
          // during callbacks.
          DOM.patch(element, callback, data)

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

      attr_callback: (
        attrptr: U32, attrlen: U32, callbackId: U32
      ) => {
        let attr = this.wisp.getString(attrptr, attrlen)
        let callback = this.callbacks.get(callbackId)
        DOM.attr(attr.toLowerCase(), callback)
      },

      close: (tagptr: U32, taglen: U32) => {
        let tag = this.wisp.getString(tagptr, taglen)
        DOM.elementClose(tag.toLowerCase())
      },

      text: (ptr: U32, len: U32) => {
        let text = this.wisp.getString(ptr, len)
        DOM.text(text)
      },

      on_keydown: (callbackId: U32) => {
        let callback = this.callbacks.get(callbackId)
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
                console.log({ result }, this.wisp.sys)
                key.preventDefault()
                key.stopImmediatePropagation()
              }
            }
          },
        )
      },

      addWindowEventHandler: (ptr: U32, len: U32, callbackId: U32) => {
        let eventName = this.wisp.getString(ptr, len)

        let callback = this.callbacks.get(callbackId)
        if (!callback) throw new Error(`no callback ${callbackId}`)

        window.addEventListener(eventName, (event: CustomEvent) => {
          callback(event.detail)
        })
      },

      prompt: (ptr: U32, len: U32) => {
        let text = this.wisp.getString(ptr, len)
        let result = prompt(text)
        if (result === null)
          return this.wisp.sys.nil
        else
          return this.wisp.newv08(result)
      },

      step: (elementId: U32, direction: U32, ctrl: U32, shift: U32) => {
        let e = this.elements.get(elementId)
        step(this.wisp, e, direction,
             (ctrl >>> 0) !== this.wisp.sys.nil,
             (shift >>> 0) !== this.wisp.sys.nil)
        e.scrollIntoView({
          behavior: "smooth",
          block: "center",
        })
      },
    }
  }
}

function step(
  wisp: Wisp,
  e: HTMLElement,
  direction: number,
  ctrl: boolean,
  shift: boolean,
): boolean {
  if (direction === 0 || direction === 1) {
    let forward = direction === 0
    let next = forward ? e.nextElementSibling : e.previousElementSibling
    if (next) {
      if (shift) {
        e.appendChild(next)
        return true
      } else {
        if (next.matches("div, article, header, main")) {
          next.insertAdjacentElement(
            forward
              ? (ctrl ? "afterend" : "afterbegin")
              : (ctrl ? "beforebegin" : "beforeend"),
            e)
          return true
        } else {
          next.insertAdjacentElement(
            forward
              ? "afterend"
              : "beforebegin",
            e)
          return true
        }
      }
    } else {
      let up = e.parentElement.closest("div, article, header, main")
      if (up) {
        up.insertAdjacentElement(forward ? "afterend" : "beforebegin", e)
        return true
      }
    }
  } else if (direction === 2) {
    let y = e.offsetTop
    let moved = false
    while (true) {
      if (step(wisp, e, 1, false, false)) {
        moved = true
        if (e.offsetTop < y)
          break
      } else {
        break
      }
    }
    return moved
  } else if (direction === 3) {
    let y = e.offsetTop
    let moved = false
    while (true) {
      if (step(wisp, e, 0, false, false)) {
        moved = true
        if (e.offsetTop > y)
          break
      } else {
        break
      }
    }
    return moved
  } else if (direction === 4) {
    if (e.previousElementSibling && e.nextElementSibling) {
      let p = e.previousElementSibling
      p.insertAdjacentElement("beforebegin", e.nextElementSibling)
      e.previousElementSibling.insertAdjacentElement("beforebegin", e)
      return true
    }
  } else if (direction === 5) {
    if (e.childNodes.length > 0) {
      e.replaceChildren()
      return true
    } else if (e.nextElementSibling) {
      e.nextElementSibling.remove()
      return true
    }
  } else if (direction === 6) {
    if (e.nextElementSibling) {
      e.parentNode.insertBefore(e.nextElementSibling.cloneNode(true), e.nextElementSibling)
      return true
    }
  } else if (direction === 7) {
    if (e.children.length > 0) {
      e.before(...[].slice.call(e.children))
      return true
    }
  } else if (direction === 8) {
    if (e.children.length === 0) {
      startEditor(e, "", (code: string) => {
        window.dispatchEvent(
          new CustomEvent("wisp-insert", { detail: wisp.newv08(code) })
        )
      })

      return true
    }
  } else if (direction === 9) {
    if (e.children.length === 0) {
      if (e.nextElementSibling) {
        evalDexp(wisp, e.nextElementSibling as HTMLElement)
        return true
      }
    } else {
      for (let d of [].slice.call(e.children)) {
        evalDexp(wisp, d as HTMLElement)
      }
      return true
    }
  } else if (direction === 10) {
    let code = domCode(document.querySelector("#file"))
    localStorage.setItem("wisp-file", code)
  }

  return false
}

function domCode(root: HTMLElement): string {
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
