import { Wisp } from "./wisp"
import { readFile } from "fs/promises"

async function start(): Promise<Wisp> {
  const wasm = await readFile("./zig-out/lib/wisp.wasm")
  const inst = await WebAssembly.instantiate(wasm, {})
  return new Wisp(inst)
}

test("basic wisp sanity check", async () => {
  const wisp = await start()

  expect(wisp.tag.int).toBe(0x00)
  expect(wisp.tag.sys).toBe(0x11)
  expect(wisp.sys.nil).toBe(0x88000000)

  const x = wisp.read("(+ 1 2 3)");
  const y = wisp.eval(x)

  expect(y).toBe(6)
})
