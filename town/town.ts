import { Wisp } from "../web/src/wisp.ts"

import Context
from "https://deno.land/std@0.130.0/wasi/snapshot_preview1.ts"
import { serve }
from "https://deno.land/std@0.130.0/http/server.ts"
import { format }
from "https://deno.land/std@0.130.0/datetime/mod.ts"
import { encode as encodeAsBase32 }
from "https://deno.land/std@0.130.0/encoding/base32.ts"

interface Run {
  wisp: Wisp
}

async function start() {
  console.info("loading Wisp WebAssembly module")

  const module = await WebAssembly.compile(
    await Deno.readFile("../core/zig-out/lib/wisp.wasm")
  )

  const runs: Record<string, Run> = {}

  serve(handler, { port: 8000 })

  console.log("listening on http://localhost:8000/")

  async function handler(req: Request): Promise<Response> {
    const url = new URL(req.url)
    const route = `${req.method} ${url.pathname}`
    let matches = null

    if (route === "POST /run") {
      const now = format(new Date(), "yyyyMMdd")
      const rnd = crypto.getRandomValues(new Uint8Array(6))
      const key = `~${now}.${encodeAsBase32(rnd).replaceAll("=", "")}`

      await Deno.mkdir(`run/${key}`, { recursive: true })

      const wasi = new Context({
        args: [],
        env: {},
        preopens: {
          ".": `run/${key}`,
        },
      })

      const instance = await WebAssembly.instantiate(module, {
        "wasi_snapshot_preview1": wasi.exports,
      })

      wasi.initialize(instance)

      const wisp = new Wisp(instance)
      runs[key] = { wisp }

      wisp.saveTape("tape")

      console.info(key, "started")

      return new Response(`/run/${key}\n`)

    } else if ((matches = route.match(/^POST \/run\/(.*?)\/eval$/))) {
      const key = matches[1]
      const run = runs[key]

      if (!run) {
        return new Response("nope", { status: 404 })
      }

      const src = await req.text()

      console.info(key, "eval", src)

      const exp = run.wisp.read(src) >>> 0
      const val = run.wisp.eval(exp) >>> 0

      return new Response(val.toString() + "\n")
    }

    return new Response("nope", { status: 404 })
  }
}

start()
