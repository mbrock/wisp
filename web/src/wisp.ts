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

type Tag =
  "int" | "chr" | "sys" | "jet" |
  "duo" | "sym" | "pkg" |
  "fun" | "mac" | "run" | "ktx" |
  "v32" | "v08"

type Sys = "t" | "nil" | "nah" | "zap" | "top"

type Kwd = "LET" | "PROMPT"

export interface WispAPI {
  memory: WebAssembly.Memory

  wisp_tag_int: WebAssembly.Global
  wisp_tag_sys: WebAssembly.Global
  wisp_tag_chr: WebAssembly.Global
  wisp_tag_jet: WebAssembly.Global
  wisp_tag_duo: WebAssembly.Global
  wisp_tag_sym: WebAssembly.Global
  wisp_tag_fun: WebAssembly.Global
  wisp_tag_mac: WebAssembly.Global
  wisp_tag_v32: WebAssembly.Global
  wisp_tag_v08: WebAssembly.Global
  wisp_tag_pkg: WebAssembly.Global
  wisp_tag_run: WebAssembly.Global
  wisp_tag_ktx: WebAssembly.Global

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
  
  wisp_heap_load_tab_col(
    heap: number, tag: number, col: number, len: number
  ): number

  wisp_heap_load_v08(heap: number, len: number): number
  wisp_heap_load_v32(heap: number, len: number): number

  wisp_heap_v08_new(heap: number, x: number, n: number): number
  wisp_heap_v08_len(heap: number): number
  wisp_heap_v08_ptr(heap: number): number
  wisp_heap_v32_len(heap: number): number
  wisp_heap_v32_ptr(heap: number): number

  wisp_dat_init(heap: number): number
  wisp_dat_read(heap: number, dat: number): void

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

  wisp_jet_name(jet: number): number
  wisp_jet_name_len(jet: number): number

  wisp_genkey(heap: number): number

  wisp_tape_save(heap: number, filename: number): number
}

export const tabs = {
  duo: ["car", "cdr"],
  sym: ["str", "pkg", "val", "fun"],
  fun: ["env", "par", "exp", "sym"],
  mac: ["env", "par", "exp", "sym"],
  v08: ["idx", "len"],
  v32: ["idx", "len"],
  pkg: ["nam", "sym", "use"],
  ktx: ["hop", "env", "fun", "acc", "arg"],
  run: ["exp", "val", "err", "env", "way"],
}

type Row = Record<string, number[]>
type Tab = Record<string, Row>

export interface Data {
  tag: Record<Tag, number>
  sys: Record<Sys, number>
  kwd: Record<Kwd, number>
  tab: Tab
  v08: ArrayBuffer
  v32: ArrayBuffer
}

export class Wisp {
  instance: WebAssembly.Instance
  api: WispAPI
  heap: number
  tag: Record<Tag, number>
  sys: Record<Sys, number>

  constructor(instance: WebAssembly.Instance) {
    this.instance = instance
    this.api = this.instance.exports as unknown as WispAPI
    this.tag = this.loadTags()
    this.sys = this.loadSys()
    this.heap = this.api.wisp_heap_init()
  }

  loadTags(): Record<Tag, number> {
    const mem = new DataView(this.api.memory.buffer)
    const tag = (x: Tag) =>
      mem.getUint8(this.api[`wisp_tag_${x}`].value)

    return {
      int: tag("int"),
      sys: tag("sys"),
      chr: tag("chr"),
      jet: tag("jet"),
      duo: tag("duo"),
      sym: tag("sym"),
      fun: tag("fun"),
      mac: tag("mac"),
      v08: tag("v08"),
      v32: tag("v32"),
      pkg: tag("pkg"),
      run: tag("run"),
      ktx: tag("ktx"),
    }
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

  loadData(): Data {
    console.log("loading data")
    return {
      sys: this.sys,
      tag: this.tag,
      tab: this.loadTab(),
      v08: this.v08slice(),
      v32: this.v32slice(),
      kwd: {
        LET: this.read("LET"),
        PROMPT: this.read("PROMPT"),
      },
    }
  }

  loadTab(): Tab {
    const datptr = this.api.wisp_dat_init(this.heap)
    this.api.wisp_dat_read(this.heap, datptr)

    const n = Object.values(tabs).length
    const m = Object.values(tabs).reduce((n, cols) => n + cols.length, 0)

    const dat = new DataView(this.api.memory.buffer, datptr, 4 * (n + m))
    const mem = new DataView(this.api.memory.buffer)

    const u32 = (v: DataView, x: number) => v.getUint32(x, true)
    const u32s = (x: number, n: number): number[] => {
      const xs = []
      for (let i = 0; i < n; i++)
        xs[i] = u32(mem, x + 4 * i)
      return xs
    }

    let i = 0
    const next = () => u32(dat, 4 * i++)

    const tab: Tab = {}

    for (const [tag, cols] of Object.entries(tabs)) {
      tab[tag] = {}
      const n = next()
      for (const col of cols) {
        tab[tag][col] = u32s(next(), n)
      }
    }

    return tab
  }

  jetName(x: number): string {
    const ptr = this.api.wisp_jet_name(x)
    const len = this.api.wisp_jet_name_len(x)
    const buf = this.api.memory.buffer.slice(ptr, ptr + len)
    return new TextDecoder().decode(buf)
  }
  
  v08len(): number {
    return this.api.wisp_heap_v08_len(this.heap)
  }

  v08ptr(): number {
    return this.api.wisp_heap_v08_ptr(this.heap) >>> 0
  }

  v32len(): number {
    return this.api.wisp_heap_v32_len(this.heap)
  }

  v32ptr(): number {
    return this.api.wisp_heap_v32_ptr(this.heap) >>> 0
  }

  v08slice() {
    const v08len = this.v08len()
    const v08ptr = this.v08ptr()
    return this.api.memory.buffer.slice(v08ptr, v08ptr + v08len)
  }
  
  v32slice() {
    const v32len = this.v32len()
    const v32ptr = this.v32ptr()
    return this.api.memory.buffer.slice(v32ptr, v32ptr + 4 * v32len)
  }

  newstr(txt: string): number {
    const buf = this.api.wisp_alloc(this.heap, txt.length + 1)
    const arr = new TextEncoder().encode(txt)
    const mem = new DataView(this.api.memory.buffer, buf, arr.length + 1)
    mem.setUint8(arr.length, 0)

    for (let i = 0; i < arr.length; i++) {
      mem.setUint8(i, arr[i])
    }

    return buf >>> 0
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

  saveTape(filename: string): void {
    const filenamePtr = this.allocString(filename)

    const result =
      this.api.wisp_tape_save(this.heap, filenamePtr) >>> 0

    if (result != this.sys.nil)
      throw new Error("wisp_tape_save failed")
    
    this.free(filenamePtr)
  }
}

export function tagOf(data: Data, x: number): Tag {
  const tagnum = x >>> (32 - 5)
  if (tagnum < 0b10000) 
    return "int"

  for (let [k, v] of Object.entries(data.tag)) {
    if (v === tagnum)
      return k as Tag
  }

  throw new Error("weird tag")
}

export function getRow(data: Data, tag: Tag, x: number): Record<string, number> {
  if (typeof x !== "number")
    throw new Error("type error")
  
  const row: Record<string, number> = {}
  const tab = data.tab[tag]
  const i = idxOf(x)

  for (const [col, xs] of Object.entries(tab)) {
    row[col] = xs[i]
  }

  return row
}

export function getUtf8String(data: Data, x: number): string {
  const { idx, len } = getRow(data, "v08", x)
  const buf = new Uint8Array(data.v08, idx, len)
  const str = new TextDecoder().decode(buf)
  return str
}

export function getV32(data: Data, x: number): Uint32Array {
  const { idx, len } = getRow(data, "v32", x)
  return new Uint32Array(data.v32, 4 * idx, len)
}

export function idxOf(x: number): number {
  return ((x >>> 0) & 0b00000111111111111111111111111111) >>> 1
}
