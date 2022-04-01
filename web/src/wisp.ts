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

  wisp_heap_new_ext(heap: number, idx: number): number
  
  wisp_heap_get_v08_ptr(heap: number, v08: number): number
  wisp_heap_get_v08_len(heap: number, v08: number): number
  
  wisp_heap_v08_new(heap: number, ptr: number, len: number): number
  wisp_heap_v32_new(heap: number, ptr: number, len: number): number
  
  wisp_start_web(heap: number): number
  
  wisp_read(heap: number, buf: number): number
  wisp_read_many(heap: number, buf: number): number
  
  wisp_eval(heap: number, exp: number, max: number): number
  wisp_eval_step(heap: number, run: number, mode: number): number
  
  wisp_call_package_function(
    heap: number,
    pkgname_ptr: number, pkgname_len: number,
    funname_ptr: number, funname_len: number,
    data: number,
  ): number

  wisp_intern_keyword(heap: number, ptr: number, len: number): number
  
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

  newExt(idx: number): number {
    return this.api.wisp_heap_new_ext(this.heap, idx)
  }

  loadString(v08: number): string {
    let ptr = this.api.wisp_heap_get_v08_ptr(this.heap, v08)
    let len = this.api.wisp_heap_get_v08_len(this.heap, v08)
    return this.getString(ptr, len)
  }
  
  getString(ptr: number, len: number): string {
    let buffer = new Uint8Array(this.api.memory.buffer, ptr, len)
    let decoder = new TextDecoder
    return decoder.decode(buffer)
  }

  getVector(ptr: number, len: number): number[] {
    let buffer = new Uint32Array(this.api.memory.buffer, ptr, len)
    return [].slice.call(Array.from(buffer)).map((x: number) => x >>> 0)
  }

  allocString(s: string): number {
    const arr = new TextEncoder().encode(s)
    const buf = this.api.wisp_alloc(this.heap, arr.length + 1)
    const mem = new Uint8Array(this.api.memory.buffer, buf, arr.length + 1)

    mem.set(arr)
    mem[arr.length] = 0

    return buf
  }

  newv08(s: string): number {
    const buf = this.allocString(s)
    const v08 = this.api.wisp_heap_v08_new(this.heap, buf, s.length)
    this.free(buf)
    return v08 >>> 0
  }
  
  newv32(arr: number[]): number {
    const buf = this.api.wisp_alloc(this.heap, 4 * arr.length)
    const mem = new Uint32Array(this.api.memory.buffer, buf, arr.length)
    mem.set(arr)
    mem[arr.length] = 0
    
    const v32 = this.api.wisp_heap_v32_new(this.heap, buf, arr.length)
    this.free(buf)
    
    return v32 >>> 0
  }

  internKeyword(s: string): number {
    const buf = this.allocString(s)
    const x = this.api.wisp_intern_keyword(this.heap, buf, s.length)
    this.free(buf)
    return x >>> 0
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

  readMany(sexp: string): number {
    const buf = this.allocString(sexp)
    const x = this.api.wisp_read_many(this.heap, buf)
    this.free(buf)
    return x >>> 0
  }

  eval(exp: number): number {
    return this.api.wisp_eval(this.heap, exp, 10000) >>> 0
  }
}
