type Tag =
  "int" | "sys" | "chr" | "fop" | "mop" |
  "duo" | "sym" | "fun" | "mac" | "v32" | "v08" | "pkg" |
  "ct0" | "ct1" | "ct2" | "ct3"

type Sys = "t" | "nil" | "nah" | "zap" | "top"

export interface WispAPI {
  memory: WebAssembly.Memory

  wisp_tag_int: WebAssembly.Global
  wisp_tag_sys: WebAssembly.Global
  wisp_tag_chr: WebAssembly.Global
  wisp_tag_fop: WebAssembly.Global
  wisp_tag_mop: WebAssembly.Global
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

  wisp_ctx_init: () => number

  wisp_dat_init: (ctx: number) => number
  wisp_dat_read: (ctx: number, dat: number) => void

  wisp_read: (ctx: number) => number
}

export class Wisp {
  instance: WebAssembly.Instance
  api: WispAPI
  mem: DataView

  tag: Record<Tag, number>
  sys: Record<Sys, number>

  ctx: number

  constructor(wasmCode: Uint8Array) {
    const wasmModule = new WebAssembly.Module(wasmCode)
    this.instance = new WebAssembly.Instance(wasmModule)
    this.api = this.instance.exports as unknown as WispAPI
    this.mem = new DataView(this.api.memory.buffer)
    this.tag = this.loadTags()
    this.sys = this.loadSys()
    this.ctx = this.api.wisp_ctx_init()
  }

  loadTags(): Record<Tag, number> {
    const tag = (x: Tag) =>
      this.mem.getUint8(this.api[`wisp_tag_${x}`].value)

    return {
      int: tag("int"),
      sys: tag("sys"),
      chr: tag("chr"),
      fop: tag("fop"),
      mop: tag("mop"),
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

  static async load(wasmPath: string): Promise<Wisp> {
    return new Wisp(await Deno.readFile(wasmPath))
  }

  readDat() {
    const datptr = this.api.wisp_dat_init(this.ctx)
    this.api.wisp_dat_read(this.ctx, datptr)

    const tabs = {
      duo: ["car", "cdr"],
      sym: ["str", "pkg", "val", "fun"],
      fun: ["env", "par", "exp"],
      mac: ["env", "par", "exp"],
      v08: ["idx", "len"],
      v32: ["idx", "len"],
      pkg: ["nam", "sym"],
      ct0: ["env", "fun", "arg", "exp", "hop"],
      ct1: ["env", "yay", "nay"],
      ct2: ["env", "exp", "hop"],
      ct3: ["env", "exp", "dew", "arg", "hop"],
    }

    const n = Object.values(tabs).length
    const m = Object.values(tabs).reduce((n, cols) => n + cols.length, 0)

    const dat = new DataView(this.api.memory.buffer, datptr, 4 * (n + m))
    const mem = new DataView(this.api.memory.buffer)

    const u32 = (v: DataView, x: number) => v.getUint32(x, true)
    const u32s = (x: number, n: number) => {      
      const xs = []
      for (let i = 0; i < n; i++)
        xs[i] = u32(mem, x + 4 * i)
      return xs
    }

    let i = 0
    const next = () => u32(dat, 4 * i++)

    const tab: Record<string, Record<string, unknown>> = {}

    for (const [tag, cols] of Object.entries(tabs)) {
      tab[tag] = {}
      const n = next()
      for (const col of cols) {
        tab[tag][col] = u32s(next(), n)
      }
    }

    return tab
  }
}
