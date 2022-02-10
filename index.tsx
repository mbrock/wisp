/*
 * This file is part of Wisp.
 *
 * Wisp is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * Wisp is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
 * Public License for more details.

 * You should have received a copy of the GNU Affero General Public
 * License along with Wisp. If not, see <https://www.gnu.org/licenses/>.
 */

import * as ReactDOM from "react-dom"

import * as React from "react"

import {
  useState, useCallback, useEffect
} from "react"

import {
  RecoilRoot, atom, useRecoilState, useRecoilValue
} from "recoil"

const NIL = {
  type: Symbol.for("SYMBOL"),
  name: "NIL",
  "function": null
}

interface EmscriptenModule {
  ccall: (arg0: string,
          arg1: string,
          arg2: string[],
          arg3: string[]) => number

  HEAPU8: {
    buffer: ArrayBufferLike
  }
}

let WispModule: EmscriptenModule

interface WispGlue {
  promise: (p: Promise<any>) => number
  promises: Record<number, Promise<any>>
}

declare global {
  interface Window {
    WispModule: EmscriptenModule
    loadWisp: any
    wisp: WispGlue
  }
}

window.wisp = {
  nextPromiseID: 1,
  promises: {},

  promise(p) {
    let id = wisp.nextPromiseID
    wisp.promises[id] = p
    return wisp.nextPromiseID++
  }
}

NIL["function"] = NIL

const Atoms = {
  lines: atom({
    key: "lines",
    default: [],
  }),

  booted: atom({
    key: "booted",
    default: false,
  }),

  heapGraph: atom({
    key: "heapGraph",
    default: {
      entries: {},
      elements: [],
    }
  }),
}

function Wisp() {
  let [, setLines] = useRecoilState(Atoms.lines)
  let [booted, setBooted] = useRecoilState(Atoms.booted)

  function print(text: string, tag: string) {
    let parts = text.split(/[«»]/)
    console.log(parts)

    let things = []

    for (let i = 0; i < parts.length; i++) {
      if (i % 2 == 0) {
        if (parts[i] !== "")
          things.push({ text: parts[i], tag })
      } else {
        let match = parts[i].match(/^.*? 0x(.*)$/)
        if (match) {
          let value = grokValue(parseInt(match[1], 16))
          things.push({ text: <ObjectView value={value} />, tag })
        }
      }
    }

    setLines(lines => [...lines, things])
  }

  useEffect(function () {
    async function load() {
      let Module = await window.loadWisp({
        printErr: (x: any) => {
          console.info(x)
          print(x, "stderr")
        },
        print(x: any) {
          console.log(x)
          print(x, "stdout")
        },
        preRun(Module: { ENV: { WISP_HEAP: string } }) {
          Module.ENV.WISP_HEAP = "/wisp/heap"
        }
      })

      console.log("loaded Wisp")
      Module.FS.mkdir("/wisp")
      Module.FS.mount(Module.IDBFS, {}, "/wisp")
      Module.FS.syncfs(true, (err: any) => {
        if (err) {
          throw err
        } else {
          console.log("syncfs loaded")
          Module.ccall("wisp_main", null, null, [])
          setBooted(true)
        }
      })

      window.WispModule = WispModule = Module
    }

    load()
  }, [])

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        gap: ".75rem",
        height: "100%"
      }}
      className={ booted ? "fade-in now" : "fade-in later" }>
      { booted
        ? <>
            <Browser />
            <REPL />
            <Explorer />
          </>
        : null }
    </div>
  )
}

function Line({ data }) {
  if (data.text) {
    return (
      <div className={data.tag}>
        {data.text}
      </div>
    )
  } else debugger
}

function lowtag(x: number) {
  return x & 7
}

function deref(x: number) {
  return x & ~7
}

let heapCache = {}

function grok(heap: DataView, x: number) {
  switch (lowtag(x)) {
  case 0:
  case 4:
    return x >> 2

  case 1:
  case 5:
  case 7:
    return grokPointer(heap, deref(x))

  case 3:
    return grokList(heap, x)

  case 2:
  case 6:
    return grokImmediate(heap, x)

  default:
    throw new Error(`lowtag ${lowtag(x)}`)
  }
}

const widetags = {
  0xC2: "INSTANCE",
  0x32: "STRING",
  0xAE: "SYMBOL",
  0xA2: "BUILTIN",
}

function grokImmediate(_heap: DataView, x: number) {
  let widetagNumber = x & ((1 << 8) - 1)
  let widetag = widetags[widetagNumber]

  switch (widetag) {
  case "BUILTIN":
    return heapCache[x] = { builtin: headerData(x) }

  default:
    throw new Error(`${widetagNumber}`)
  }
}

function grokPointer(heap: DataView, x: number) {
  if (heapCache[x])
    return heapCache[x]

  let header = heap.getUint32(x, true)
  let widetagNumber = header & ((1 << 8) - 1)
  let widetag = widetags[widetagNumber]

  switch (widetag) {
  case "INSTANCE":
    return grokInstance(heap, x, headerData(header) - 1, x + 4)

  case "SYMBOL":
    return grokSymbol(heap, x)

  case "STRING":
    return grokString(heap, headerData(header), x + 4)

  default:
    debugger
    throw new Error(widetag)
  }
}

function headerData(x: number) {
  return x >> 8
}

function grokInstance(heap: DataView, address: number, slotCount: number, x: number) {
  let value = {
    type: Symbol.for("INSTANCE"),
    address,
    klass: undefined,
    slots: undefined,
  }

  heapCache[x] = value

  value.klass = grok(heap, heap.getUint32(x, true))

  let slots = []
  for (let i = 0; i < slotCount; i++) {
    let value = grok(heap, heap.getUint32(x + 4 * (i + 1), true))
    slots.push(value)
  }

  value.slots = slots

  return value
}

function grokSymbol(heap: DataView, x: number) {
  let value = heapCache[x] = {
    type: Symbol.for("SYMBOL"),
    name: undefined,
    "function": undefined,
    value: undefined,
  }

  value.name = grok(heap, heap.getUint32(x + 4 * 4, true))
  value.value = grok(heap, heap.getUint32(x + 1 * 4, true))
  value["function"] = grok(heap, heap.getUint32(x + 4 * 6, true))

  return value
}

function grokString(heap: DataView, length: number, x: number) {
  let decoder = new TextDecoder
  return decoder.decode(
    heap.buffer.slice(
      heap.byteOffset + x,
      heap.byteOffset + x + length,
    )
  )
}

function grokList(heap: DataView, x: number) {
  if (x === 3) {
    return NIL
  }

  let y = deref(x)
  return {
    type: Symbol.for("CONS"),
    car: grok(heap, heap.getUint32(y, true)),
    cdr: grok(heap, heap.getUint32(y + 4, true)),
  }
}

function grokValue(x: number) {
  let heapBase = WispModule.ccall(
    "wisp_get_heap_pointer", null, ["u8*"], [])

  let heap = new DataView(
    WispModule.HEAPU8.buffer,
    heapBase,
    4 * 1024 * 1024)

  return grok(heap, x)
}

function listElements(
  cons: {
    car?: any; cdr?: any;
    type?: symbol;
    name?: string;
    function?: any }) {

  let items = []
  while (cons != NIL) {
    items.push(cons.car)
    cons = cons.cdr
  }

  return items
}

function sortBy (list: any[], key: string) {
  return list.concat().sort(
    (a: { [x: string]: number }, b: { [x: string]: number }) => (a[key] > b[key])
      ? 1
      : ((b[key] > a[key])
         ? -1
         : 0)
  )
};

function SymbolList({ title, symbols }) {
  if (symbols.length === 0)
    return null
  else
    return (
      <section>
        <header>{title}</header>
        <ul>
          {
            symbols.map((x: any, i: number) =>
              <li key={i}>
                {x.name}
              </li>)
          }
        </ul>
      </section>
    )
}

function Package({ instance }) {
  let allSymbols = sortBy(
    listElements(instance.slots[1]),
    "name"
  )

  let builtins = []
  let functions = []
  let macros = []
  let variables = []
  let symbols = []

  for (let symbol of allSymbols) {
    let f = symbol["function"]
    let v = symbol.value
    if (v !== NIL) {
      variables.push(symbol)
    } else if (f !== NIL) {
      if (typeof f.builtin === "number") {
        builtins.push(symbol)
      } else if (f.slots[3] !== NIL) {
        macros.push(symbol)
      } else {
        functions.push(symbol)
      }
    } else {
      symbols.push(symbol)
    }
  }

  return (
    <div>
      <section>
        <SymbolList title="Macros" symbols={macros} />
        <SymbolList title="Functions" symbols={functions} />
        <SymbolList title="Builtins" symbols={builtins} />
        <SymbolList title="Variables" symbols={variables} />
      </section>
    </div>
  )
}

function Browser() {
  let booted = useRecoilValue(Atoms.booted)

  let [value, setValue] = React.useState(null)
  let [_heapGraph, setHeapGraph] = useRecoilState(Atoms.heapGraph)

  React.useEffect(() => {
    if (!booted) return

    setValue(grokValue(WispModule.ccall(
      "wisp_get_root_package", null, [], [])))

    setHeapGraph(makeHeapGraph())

  }, [booted])

  return (
    <div className="browser">
      <header className="titlebar">
        <span>
          <b>WISP</b>
        </span>
        <span>
          Package
        </span>
      </header>
      <div className="bg p-1 scroll-y">
        {value ? <Package instance={value} /> : "..."}
      </div>
    </div>
  )
}

const slotNames = {
  CLOSURE: ["Params", "Code", "Scopes", "Macro"],
  "DOM-ELEMENT": ["Tag", "Attributes", "Body"],
}

function slotName(value: { klass: { name: string | number } }, i: string | number) {
  let names = slotNames[value.klass.name]
  return names ? names[i] : `Slot ${i}`
}


function ObjectView({ value }) {
  let [expanded, setExpanded] = React.useState(false)

  let toggle = React.useCallback((e) => {
    e.stopPropagation()
    setExpanded(x => !x)
  }, [setExpanded])

  if (value.type === Symbol.for("INSTANCE")) {
    if (expanded)
      return (
        <table className="instance" onClick={toggle}>
          <tbody>
            <tr>
              <td>Class</td>
              <td><ObjectView value={value.klass} /></td>
            </tr>
            {
              value.slots.map((slot: any, i: React.Key) => (
                <tr key={i}>
                  <td>{slotName(value, i)}</td>
                  <td><ObjectView value={slot} /></td>
                </tr>
              ))
            }
          </tbody>
        </table>
      )
    else
      return (
        <div className="instance" onClick={toggle}>
          <ObjectView value={value.klass} />
          {` @${value.address.toString(10)}`}
        </div>
      )

  } else if (value.type === Symbol.for("SYMBOL")) {
    return (
      <span className="symbol">{value.name}</span>
    )

  } else if (value.type === Symbol.for("CONS")) {
    let items = []
    let list = value
    let last: any

    while (true) {
      items.push(list.car)
      list = list.cdr

      if (list === NIL) {
        break
      } else if (list.type != Symbol.for("CONS")) {
        last = list
        break
      }
    }

    return (
      <div className="list">
        {items.map((x, i) => <ObjectView value={x} key={i} />)}{
          last ? <span>. <ObjectView value={last} /></span> : null
        }
      </div>
    )

  } else if (value === NIL) {
    return <span className="symbol">NIL</span>

  } else if (typeof value === "number") {
    return <span className="number">{value}</span>

  } else if (typeof value.builtin === "number") {
    return (
      <span className="builtin">
        {`«BUILTIN ${value.builtin}»`}
      </span>
    )

  } else {
    return <span className="string">{`"${value}"`}</span>
  }
}

function REPL() {
  let [_heapGraph, setHeapGraph] = useRecoilState(Atoms.heapGraph)
  let [lines, setLines] = useRecoilState(Atoms.lines)

  let [input, setInput] = useState("")

  let outputRef = React.useRef(null)

  let handleChange = useCallback(e => {
    setInput(e.target.value)
  }, [])

  async function evalCode(code: string) {
    console.log(code)
    let result

    let awaited = WispModule.ccall(
      "wisp_eval_code_async",
      "number",
      ["string"],
      [code]
    )

    while (awaited) {
      let promiseId = WispModule.ccall(
        "wisp_get_promise_id",
        "number",
        [], []
      )

      let promise = wisp.promises[promiseId]
      console.log("awaiting", promise)

      let jsString = await promise

      let lengthBytes =
        WispModule.lengthBytesUTF8(jsString) + 1

      let stringOnWasmHeap =
        WispModule._malloc(lengthBytes)

      WispModule.stringToUTF8(jsString, stringOnWasmHeap, lengthBytes)

      awaited = WispModule.ccall(
        "wisp_resume_await",
        "number",
        ["u8*"],
        [stringOnWasmHeap]
      )

      WispModule._free(stringOnWasmHeap)
    }

    setHeapGraph(makeHeapGraph())

    let sexp = grokValue(
      WispModule.ccall(
        "wisp_read_from_string",
        "number",
        ["string"],
        [code]
      )
    )

    console.log(result)

    let value = grokValue(result)
    console.log(value)

    setLines(lines => [
      ...lines, [
        {
          text: <ObjectView value={sexp} />,
          tag: "stdin"
        },
        {
          text: "→",
        },
        {
          text: <ObjectView value={value} />,
          tag: "stdout"
        }
      ]
    ])

  }

  let handleSubmit = useCallback(e => {
    e.preventDefault()
    evalCode(input)
    setInput("")
  }, [input])

  useEffect(() => {
    outputRef.current.scrollTop = outputRef.current.scrollHeight
  }, [lines])

  return (
    <div id="repl">
      <header className="titlebar">
        <span>
          <b>Notebook</b>
        </span>
        <span>
          Package: <em>WISP</em>
        </span>
      </header>
      <div id="output" ref={outputRef}>
        {
          lines.map((xs, i) =>
            <div key={i} className="chunk">
              {
                xs.map((x: any, j: React.Key) =>
                  <Line data={x} key={j} />
                )
              }
            </div>
          )
        }
      </div>
      <form id="form" onSubmit={handleSubmit}>
        <input
          id="input" autoFocus autoComplete="off"
          onChange={handleChange}
          value={input}
        />
      </form>
    </div>
  )
}

type HeapEntry = {
  offset: number,
  type: "string" | "instance" | "cons" | "symbol" | "builtin",
  slots: number[],
  value: string | number | null
}

function makeHeapGraph() {
  let heapBase = WispModule.ccall(
    "wisp_get_heap_pointer", null, ["u8*"], [])

  const heapSize = 64 * 1024

  let heap = new DataView(
    WispModule.HEAPU8.buffer,
    heapBase,
    heapSize)

  let scan = 0
  let elements = []
  let nodes = {}
  let entries: Record<number, HeapEntry> = {}

  let u32 = (x: number) => heap.getUint32(x, true)
  let align = (i: number) => (i + 7) & ~7

  function addNode(label: string) {
    let node = nodes[`${scan}`] = {
      data: {
        id: `${scan}`,
        label
      }
    }

    elements.push(node)
  }

  function addEdge(source: number, target: number) {
    elements.push({
      data: {
        id: `${source}-${target}`,
        source: `${source}`,
        target: `${target}`,
      }
    })
  }

  while (scan < heapSize) {
    let header = scan
    let thing = u32(header)

    if (thing === 0) {
      scan += 8
    } else {
      let tag = thing & 0xff
      switch (tag) {
      case 0xC2:
      case 0xAE:
        {
          addNode(tag == 0xC2 ? "instance" : "symbol")

          let n = thing >> 8
          let slots = []

          for (let i = 1; i < n; i++) {
            let entry = u32(header + i * 4)
            if ((entry & 1) === 1) {
              addEdge(scan, entry & ~7)
              slots.push(entry)
            }
          }

          entries[scan] = {
            offset: scan,
            type: tag == 0xC2 ? "instance" : "symbol",
            slots,
            value: null,
          } as HeapEntry

          scan += align((1 + n) * 4)

          break
        }

      case 0x32:
        {
          let n = thing >> 8

          entries[scan] = {
            offset: scan,
            type: "string",
            slots: [],
            value: grokString(heap, n, scan + 4),
          }

          addNode(`"${grokString(heap, n, scan + 4)}"`)
          scan += align(4 + n + 1)

          break
        }

      default:
        {
          addNode("cons")

          let slots = []

          let n = 2
          for (let i = 0; i < n; i++) {
            let entry = u32(header + i * 4)
            slots.push(entry)
            if ((entry & 1) === 1)
              addEdge(scan, entry & ~7)
          }

          entries[scan] = {
            offset: scan,
            type: "cons",
            slots,
            value: null,
          }

          scan += 8
          break
        }
      }
    }
  }

  console.log(entries)

  return { entries, elements }
}

function Slot(
  { entries, value }: { entries: Record<string, HeapEntry>, value: number }
) {
  if (value == 3)
    return <span>nil</span>

  if (value & 1) {
    let entry = entries[value & ~7]
    if (!entry)
      return <span style={{ color: "red" }}>({value} missing)</span>
    else {
      return (
        <a
          style={{textDecoration: "none"}}
          href={`#heap-${value & ~7}`}>
          ({value & ~7} {entry.type})
        </a>
      )
    }
  } else {
    return <span>{value}</span>
  }
}

function HeapView({ entries } : { entries: Record<string, HeapEntry> }) {
  let [_heapGraph, setHeapGraph] = useRecoilState(Atoms.heapGraph)

  function gc() {
    WispModule.ccall("wisp_tidy", null, [], [])
    setHeapGraph(makeHeapGraph())
  }

  return (
    <div className="flex column bg hide">
      <header className="titlebar">
        <span>
          <b>Heap Inspector</b>
        </span>
        <button onClick={gc} className="px">
          Collect Garbage
        </button>
      </header>
      <div className="flex column gap scroll-y">
        {makeHeapEntryViews()}
      </div>
    </div>
  )

  function makeHeapEntryViews(): React.ReactNode {
    return Object.entries(entries).map(([i, entry]) =>
      <div className="flex" key={i}>
        <div style={{
          width: "4rem",
          textAlign: "right",
          paddingRight: "0.5rem",
          opacity: 0.5,
        }}>{`${entry.offset} `}</div>
        <div key={i} className="flex row gap wrap" id={`heap-${i}`}>
          <div>{`${entry.type}`}</div>
            {entry.slots.map((slot, j) =>
              <Slot key={j} entries={entries} value={slot} />
            )}
          <div>{`${entry.value === null ? "" : entry.value}`}</div>
        </div>
      </div>
    )
  }
}

function Explorer() {
  let [heapGraph, setHeapGraph] = useRecoilState(Atoms.heapGraph)

  let { entries } = heapGraph

  let makeHeapGraphButton =
    <button onClick={_e => setHeapGraph(makeHeapGraph())}>
      Load heap
    </button>

  let heapView =
    <HeapView entries={entries} />

  if (Object.keys(entries).length === 0) {
    return makeHeapGraphButton
  } else {
    return heapView
  }
}

onload = () => {
  ReactDOM.render(
    <RecoilRoot>
      <Wisp />
    </RecoilRoot>,
    document.querySelector("#app")
  )
}
