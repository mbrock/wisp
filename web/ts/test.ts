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

import { Wisp } from "./wisp"
import { readFile } from "fs/promises"
import { WASI } from "wasi"

async function start(): Promise<Wisp> {
  const wasi = new WASI({
    args: [],
    env: {},
    preopens: {},
  })

  const source = await readFile("./zig-out/lib/wisp.wasm")
  const wasm = await WebAssembly.compile(source)
  const result = await WebAssembly.instantiate(wasm, {
    wasi_snapshot_preview1: wasi.wasiImport
  })

  wasi.initialize(result)

  return new Wisp(result)
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
