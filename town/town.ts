import { Wisp } from "../web/src/wisp.ts"

import Context
from "https://deno.land/std@0.130.0/wasi/snapshot_preview1.ts"
import { serve }
from "https://deno.land/std@0.130.0/http/server.ts"
import { format }
from "https://deno.land/std@0.130.0/datetime/mod.ts"
import { encode as encodeAsBase32 }
from "https://deno.land/std@0.130.0/encoding/base32.ts"

interface Heap {
  wisp: Wisp
}

async function loadWispModule(): Promise<WebAssembly.Module> {
  console.info("loading Wisp WebAssembly module")
  return WebAssembly.compile(
    await Deno.readFile("../core/zig-out/lib/wisp.wasm")
  )
}

class Town {
  heapsByKey: Record<string, Heap> = {}
  webSockets: Set<WebSocket> = new Set<WebSocket>()

  constructor(
    public wispModule: WebAssembly.Module
  ) {}

  async handle(req: Request): Promise<Response> {
    const url = new URL(req.url)
    const route = `${req.method} ${url.pathname}`
    let matches = null

    if (route === "GET /") {
      return this.getRoot()
    } else if (route === "GET /websocket") {
      return this.getWebsocket(req)
    } else if (route === "POST /run") {
      return await this.postRun()
    } else if ((matches = route.match(/^POST \/run\/(.*?)\/eval$/))) {
      return await this.postEval(req, matches[1])
    } else {
      return new Response("nope", { status: 404 })
    }
  }

  getRoot(): Response {
    return new Response(`
      <script>
      let ws = new WebSocket("ws://"+location.host+"/websocket")
      ws.onmessage = e => pre.textContent += e.data+"\\n"
      </script>
      <input onkeyup="event.key=='Enter'&&ws.send(this.value)"><pre id=pre>
    `, {
      headers: { "content-type": "text/html" }
    })
  }

  getWebsocket(req: Request): Response {
    const { socket, response } = Deno.upgradeWebSocket(req)

    this.webSockets.add(socket)

    socket.onopen = () => {
      console.log("new WebSocket")
    }

    socket.onmessage = (e: MessageEvent) => {
      console.log("WebSocket data:", e.data)
    }

    socket.onerror = e => console.error(e)

    return response
  }

  async postRun(): Promise<Response> {
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

    const instance = await WebAssembly.instantiate(this.wispModule, {
      "wasi_snapshot_preview1": wasi.exports,
    })

    wasi.initialize(instance)

    const wisp = new Wisp(instance)
    this.heapsByKey[key] = { wisp }

    wisp.saveTape("tape")

    console.info(key, "started")

    return new Response(`/run/${key}\n`)
  }

  async postEval(req: Request, key: string): Promise<Response> {
    const run = this.heapsByKey[key]

    if (!run) {
      return new Response("nope", { status: 404 })
    }

    const src = await req.text()

    console.info(key, "eval", src)

    const exp = run.wisp.read(src) >>> 0
    const val = run.wisp.eval(exp) >>> 0

    return new Response(val.toString() + "\n")
  }
}

async function start() {
  const wispModule = await loadWispModule()
  const town = new Town(wispModule)

  serve(req => town.handle(req), { port: 8000 })

  console.log("listening on http://localhost:8000/")

}

start()
