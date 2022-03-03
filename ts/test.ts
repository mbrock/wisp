import { assertEquals } from "https://deno.land/std@0.127.0/testing/asserts.ts"

import { Wisp } from "./wisp.ts"

Deno.test("wisp", { permissions: { read: true }}, async () => {
  const wisp = await Wisp.load("zig-out/lib/wisp.wasm")

  assertEquals(wisp.tag.int, 0x00)
  assertEquals(wisp.tag.sys, 0x11)

  assertEquals(wisp.sys.nil, 0x88000000)

  console.log(wisp.readDat())
})
