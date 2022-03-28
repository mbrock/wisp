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

import "./index.css"
import "../vendor/inter/inter.css"

import wispWasmPath from "wisp.wasm"

import { Wisp, WispAPI } from "./wisp"
import { WASI } from "./wasi"
import { WASD, Callback } from "./wasd"
import { startEditor } from "./edit"

import initialDocument from "./dexp.wisp" 

type U32 = number

onload = async () => {
  const wasi = new WASI
  const wasd = new WASD

  const { instance } = await WebAssembly.instantiateStreaming(
    fetch(wispWasmPath), {
      wasi_snapshot_preview1: wasi.exports(),
      dom: wasd.exports(),
    }
  )

  const exports = instance.exports as unknown as WispAPI

  wasi.setMemory(exports.memory)

  let ctx: Wisp | null = null

  wasd.setCallbackOperation(
    ({ packageName, functionName }: Callback, data: U32) => {
      let pkgname = ctx.allocString(packageName)
      let funname = ctx.allocString(functionName)
      console.info("callback", packageName, functionName, data)
      ctx.api.wisp_call_package_function(
        ctx.heap,
        pkgname, packageName.length,
        funname, functionName.length,
        data,
      )

      ctx.free(pkgname, funname)
    })

  ctx = new Wisp(instance)
  
  wasd.setWisp(ctx)

  if (ctx.api.wisp_start_web(ctx.heap) >>> 0 != ctx.sys.t)
    throw new Error("wisp start web failed")

  function exec(code: string, how: "run" | "debug") {
    const src = ctx.read(`(progn\n${code}\n)`)
    const run = ctx.api.wisp_run_init(ctx.heap, src)

    if (how == "run")
      ctx.api.wisp_run_eval(ctx.heap, run, 4_000_000)
  }

  startEditor(
    document.querySelector("#wisp-editor"),
    exec,
    initialDocument,
  )
}
