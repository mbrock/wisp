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

import wispWasmPath from "wisp.wasm"

import { Wisp, WispAPI } from "./wisp"
import { WASI } from "./wasi"
import { WASD, domCode } from "./wasd"
import { startEditor } from "./edit"

import dexpBase from "./dexp.wisp"
import initialDocument from "./user.wisp"

import FS from "@isomorphic-git/lightning-fs"
import * as Git from "isomorphic-git"
import http from "isomorphic-git/http/web"
import { Buffer } from "buffer"

declare global {
    interface Window {
      fs: FS
      git: typeof Git
      git_http: typeof http
      wisp: {
        startEditor: typeof startEditor,
        domCode: typeof domCode,
      }
    }
}

window.fs = new FS("wisp")
window.git = Git
window.git_http = http
window.Buffer = Buffer
window.wisp = {
  startEditor,
  domCode,
}

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

  ctx = new Wisp(instance)

  wasd.setWisp(ctx)

  if (ctx.api.wisp_start_web(ctx.heap) >>> 0 != ctx.sys.t)
    throw new Error("wisp start web failed")

  function exec(code: string) {
    const src = ctx.read(`(progn\n${code}\n)`)
    const run = ctx.api.wisp_run_init(ctx.heap, src)
    ctx.api.wisp_run_eval(ctx.heap, run, 4_000_000)
  }

  exec(dexpBase)
  exec(initialDocument)
  const file = localStorage.getItem("wisp-file") || initialDocument
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
