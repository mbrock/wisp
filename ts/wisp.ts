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
  "int" | "sys" | "chr" | "jet" |
  "duo" | "sym" | "fun" | "mac" | "v32" | "v08" | "pkg" |
  "ct0" | "ct1" | "ct2" | "ct3"

type Sys = "t" | "nil" | "nah" | "zap" | "top"

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
  wisp_tag_ct0: WebAssembly.Global
  wisp_tag_ct1: WebAssembly.Global
  wisp_tag_ct2: WebAssembly.Global
  wisp_tag_ct3: WebAssembly.Global

  wisp_sys_t: WebAssembly.Global
  wisp_sys_nil: WebAssembly.Global
  wisp_sys_nah: WebAssembly.Global
  wisp_sys_zap: WebAssembly.Global
  wisp_sys_top: WebAssembly.Global

  wisp_alloc(ctx: number, n: number): number
  wisp_free(ctx: number, x: number): void
  wisp_destroy(ctx: number, x: number): void

  wisp_ctx_init(): number

  wisp_ctx_v08_len(ctx: number): number
  wisp_ctx_v08_ptr(ctx: number): number
  wisp_ctx_v32_len(ctx: number): number
  wisp_ctx_v32_ptr(ctx: number): number

  wisp_dat_init(ctx: number): number
  wisp_dat_read(ctx: number, dat: number): void

  wisp_read(ctx: number, buf: number): number
  wisp_eval(ctx: number, exp: number, max: number): number
}

export class View {
  tab: Record<string, Record<string, number[]>>
  v08: ArrayBuffer
  v32: ArrayBuffer
  mem: WebAssembly.Memory
  api: WispAPI

  constructor(
    public ctx: Wisp,
  ) {
    this.api = ctx.api
    this.mem = ctx.api.memory
    this.tab = this.readTab()
    this.v08 = this.readV08()
    this.v32 = this.readV32()
  }

  readV08() {
    const v08len = this.ctx.v08len()
    const v08ptr = this.ctx.v08ptr()
    return this.api.memory.buffer.slice(v08ptr, v08ptr + v08len)
  }

  readV32() {
    const v32len = this.ctx.v32len()
    const v32ptr = this.ctx.v32ptr()
    return this.api.memory.buffer.slice(v32ptr, v32ptr + 4 * v32len)
  }

  readTab() {
    const datptr = this.api.wisp_dat_init(this.ctx.ctx)
    this.api.wisp_dat_read(this.ctx.ctx, datptr)

    const tabs = {
      duo: ["car", "cdr"],
      sym: ["str", "pkg", "val", "fun"],
      fun: ["env", "par", "exp"],
      mac: ["env", "par", "exp"],
      v08: ["idx", "len"],
      v32: ["idx", "len"],
      pkg: ["nam", "sym", "use"],
      ct0: ["env", "fun", "arg", "exp", "hop"],
      ct1: ["env", "yay", "nay", "hop"],
      ct2: ["env", "exp", "hop"],
      ct3: ["env", "exp", "dew", "arg", "hop"],
    }

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

    const tab: Record<string, Record<string, number[]>> = {}

    for (const [tag, cols] of Object.entries(tabs)) {
      tab[tag] = {}
      const n = next()
      for (const col of cols) {
        tab[tag][col] = u32s(next(), n)
      }
    }

    return tab
  }

  row(tag: Tag, x: number): Record<string, number> {
    const row: Record<string, number> = {}
    const tab = this.tab[tag]
    const i = idxOf(x)

    for (const [col, xs] of Object.entries(tab)) {
      row[col] = xs[i]
    }

    return row
  }

  str(x: number): string {
    const { idx, len } = this.row("v08", x)
    const buf = new Uint8Array(this.v08, idx, len)
    return new TextDecoder().decode(buf)
  }
}

export class Wisp {
  instance: WebAssembly.Instance
  api: WispAPI
  ctx: number
  tag: Record<Tag, number>
  sys: Record<Sys, number>

  constructor(instance: WebAssembly.Instance) {
    this.instance = instance
    this.api = this.instance.exports as unknown as WispAPI
    this.tag = this.loadTags()
    this.sys = this.loadSys()
    this.ctx = this.api.wisp_ctx_init()
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
      ct0: tag("ct0"),
      ct1: tag("ct1"),
      ct2: tag("ct2"),
      ct3: tag("ct3"),
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

  view(): View {
    return new View(this)
  }

  v08len(): number {
    return this.api.wisp_ctx_v08_len(this.ctx)
  }

  v08ptr(): number {
    return this.api.wisp_ctx_v08_ptr(this.ctx)
  }

  v32len(): number {
    return this.api.wisp_ctx_v32_len(this.ctx)
  }

  v32ptr(): number {
    return this.api.wisp_ctx_v32_ptr(this.ctx)
  }

  read(sexp: string): number {
    const buf = this.api.wisp_alloc(this.ctx, sexp.length + 1)
    const arr = new TextEncoder().encode(sexp)
    const mem = new DataView(this.api.memory.buffer, buf, arr.length + 1)

    mem.setUint8(arr.length, 0)

    for (let i = 0; i < arr.length; i++) {
      mem.setUint8(i, arr[i])
    }

    const x = this.api.wisp_read(this.ctx, buf)
    this.api.wisp_free(this.ctx, buf)
    return x
  }

  eval(exp: number): number {
    return this.api.wisp_eval(this.ctx, exp, 10000)
  }
 
  tagOf(x: number): Tag {
    const tagnum = x >>> (32 - 5)
    if (tagnum < 0b10000) 
      return "int"

    for (let [k, v] of Object.entries(this.tag)) {
      if (v === tagnum)
        return k as Tag
    }

    throw new Error("weird tag")
  }
}

export function idxOf(x: number): number {
  return ((x >>> 0) & 0b00000111111111111111111111111111) >>> 1
}
