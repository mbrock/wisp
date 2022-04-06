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

import Context
  from "https://deno.land/std@0.127.0/wasi/snapshot_preview1.ts"

import * as DOM
  from "https://deno.land/x/deno_dom/deno-dom-wasm.ts"

import { Wisp, WASD }
  from "./wisp.js"

globalThis.NodeList = DOM.NodeList
globalThis.HTMLCollection = DOM.HTMLCollection
globalThis.document = (new DOM.DOMParser()).parseFromString(
  "<div id=wisp-app></div>",
  "text/html",
)

globalThis.wisp = {
  "import": x => import(x),
}

const wasd = new WASD
const wasmCode = await Deno.readFile(
  "../core/zig-out/lib/wisp.wasm")
const wasmModule = new WebAssembly.Module(wasmCode)
const wasiContext = new Context({ args: [], env: {} })
const wasmInstance = new WebAssembly.Instance(wasmModule, {
  wasi_snapshot_preview1: wasiContext.exports,
  dom: wasd.exports(),
})

wasiContext.initialize(wasmInstance)
const wisp = new Wisp(wasmInstance)
wasd.setWisp(wisp)

function exec(code) {
  const src = wisp.read(`(progn\n${code}\n)`)
  const run = wisp.api.wisp_run_init(wisp.heap, src)
  return wisp.api.wisp_run_eval(wisp.heap, run, 4_000_000)
}

async function load(filename) {
  const decoder = new TextDecoder("utf-8")
  const script = decoder.decode(await Deno.readFile(filename))
  const result = exec(script)
  if (result === wisp.sys.zap)
    throw new Error("zap")

  else return result >>> 0
}

await load("dexp.wisp")
await load(Deno.args[0])
