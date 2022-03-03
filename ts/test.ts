import { assertEquals } from "https://deno.land/std@0.127.0/testing/asserts.ts"

import { Wisp } from "./wisp.ts"

Deno.test("wisp", { permissions: { read: true }}, async () => {
  const code = await Deno.readFile("zig-out/lib/wisp.wasm")
  const wasm = await WebAssembly.instantiate(code, {})
  const wisp = new Wisp(wasm)

  assertEquals(wisp.tag.int, 0x00)
  assertEquals(wisp.tag.sys, 0x11)
  assertEquals(wisp.sys.nil, 0x88000000)

  const x = wisp.read("(+ 1 2 3)");
  const y = wisp.eval(x)

  assertEquals(y, 6)
})
