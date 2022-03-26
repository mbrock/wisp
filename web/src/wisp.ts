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

type Sys = "t" | "nil" | "nah" | "zap" | "top"

export interface WispAPI {
  memory: WebAssembly.Memory

  wisp_sys_t: WebAssembly.Global
  wisp_sys_nil: WebAssembly.Global
  wisp_sys_nah: WebAssembly.Global
  wisp_sys_zap: WebAssembly.Global
  wisp_sys_top: WebAssembly.Global

  wisp_alloc(heap: number, n: number): number
  wisp_free(heap: number, x: number): void
  wisp_destroy(heap: number, x: number): void

  wisp_heap_init(): number
  wisp_start_web(heap: number): number
  
  wisp_read(heap: number, buf: number): number
  
  wisp_eval(heap: number, exp: number, max: number): number
  wisp_eval_step(heap: number, run: number, mode: number): number
  
  wisp_call_package_function(
    heap: number,
    pkgname_ptr: number, pkgname_len: number,
    funname_ptr: number, funname_len: number,
    data: number,
  ): number

  wisp_run_init(heap: number, exp: number): number
  wisp_run_eval(heap: number, run: number, max: number): number
  wisp_run_restart(heap: number, run: number, exp: number): number
}

export class Wisp {
  instance: WebAssembly.Instance
  api: WispAPI
  heap: number
  sys: Record<Sys, number>

  constructor(instance: WebAssembly.Instance) {
    this.instance = instance
    this.api = this.instance.exports as unknown as WispAPI
    this.sys = this.loadSys()
    this.heap = this.api.wisp_heap_init()
  }

  loadSys(): Record<Sys, number> {
    const mem = new DataView(this.api.memory.buffer)
    const u32 = (x: number) => mem.getUint32(x, true)

    const sys = (x: Sys) =>
      u32(this.api[`wisp_sys_${x}`].value)

    return {
      t: sys("t"),
      nil: sys("nil"),
      nah: sys("nah"),
      zap: sys("zap"),
      top: sys("top")
    }
  }

  allocString(s: string): number {
    const arr = new TextEncoder().encode(s)
    const buf = this.api.wisp_alloc(this.heap, arr.length + 1)
    const mem = new Uint8Array(this.api.memory.buffer, buf, arr.length + 1)

    mem.set(arr)
    mem[arr.length] = 0

    return buf
  }

  free(...xs: number[]): void {
    for (const x of xs) {
      this.api.wisp_free(this.heap, x)
    }
  }

  read(sexp: string): number {
    const buf = this.allocString(sexp)
    const x = this.api.wisp_read(this.heap, buf)
    this.free(buf)
    return x >>> 0
  }

  eval(exp: number): number {
    return this.api.wisp_eval(this.heap, exp, 10000) >>> 0
  }
}
