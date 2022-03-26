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

import { Wisp, WispAPI } from "./wisp"
import { WASI } from "wasi"
import { WASD } from "./wasd"

import { readFile } from "fs/promises"

async function start(): Promise<Wisp> {
  const wasd = new WASD
  const wasi = new WASI({
    args: [],
    env: {},
    preopens: {},
  })

  const source = await readFile("../core/zig-out/lib/wisp.wasm")
  const wasm = await WebAssembly.compile(source)
  const result = await WebAssembly.instantiate(wasm, {
    wasi_snapshot_preview1: wasi.wasiImport,
    dom: wasd.exports(),
  })

  const exports = result.exports as unknown as WispAPI

  wasi.initialize(result)
  wasd.setMemory(exports.memory)

  return new Wisp(result)
}

test("basic wisp sanity check", async () => {
  const wisp = await start()

  expect(wisp.sys.nil).toBe(0x88000000)

  const x = wisp.read("(+ 1 2 3)");
  const y = wisp.eval(x)

  expect(y).toBe(6)
})
