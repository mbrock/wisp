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

import { Wisp, WASD }
  from "./wisp.js"
import WASI
  from "./wasi.js"

globalThis.NodeList = class NodeList {}
globalThis.HTMLCollection = class HTMLCollection {}

globalThis.wisp = {
  "import": x => {
    return import(x)
  },
}

const wasd = new WASD
const wasmCode = await Deno.readFile(
  "../core/zig-out/bin/wisp.wasm")
const wasmModule = new WebAssembly.Module(wasmCode)
const wasi = new WASI()
const wasmInstance = new WebAssembly.Instance(wasmModule, {
  wasi_snapshot_preview1: wasi.exports(),
  dom: wasd.exports(),
})

wasi.setMemory(wasmInstance.exports.memory)
const wisp = new Wisp(wasmInstance)
wasd.setWisp(wisp)

function exec(code) {
  const src = wisp.read(`
   (do
     ${code}
   )
  `)
  const run = wisp.api.wisp_run_init(wisp.heap, src)
  const result = wisp.api.wisp_run_eval(wisp.heap, run, 0) >>> 0
  if (result === wisp.sys.zap)
    throw new Error("zap")
  else
    return result
}

async function load(filename) {
  console.log(";;;; loading", filename)
  const decoder = new TextDecoder("utf-8")
  let script = decoder.decode(await Deno.readFile(filename))
  return exec(script)
}

await load("js.wisp")
await load("deno-base.wisp")
await exec(`
(async
 (fn ()
  (with-simple-error-handler (fn () (do
   (load "${Deno.args[0]}")
)))))
`)
