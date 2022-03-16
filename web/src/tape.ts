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

import { Wisp, tabs } from "./wisp"
import { Note } from "./index"
import { IDBPDatabase, openDB } from "idb"

const version = 2

type Book = [Note]

type Tab = Record<string, Record<string, ArrayBuffer>>

type Tape = {
  book: Book,
  tab: Tab,
  v08: ArrayBuffer,
  v32: ArrayBuffer,
}

async function openTapeDB(): Promise<IDBPDatabase> {
  return await openDB("wisp-v0.8", version, {
    upgrade(db) {
      db.createObjectStore("tape", { keyPath: "name" })
    },
  })
}

export async function save(tape: Tape, name: string): Promise<any> {
  const db = await openTapeDB()
  return await db.put("tape", { tape, name })
}

export async function load(name: string): Promise<Tape | null> {
  const db = await openTapeDB()
  return (await db.get("tape", name))
}

function transfer(ctx: Wisp, src: ArrayBuffer, dst: number): void {
  const mem = new Uint8Array(ctx.api.memory.buffer, dst, src.byteLength)
  mem.set(new Uint8Array(src))
 }

export function play(ctx: Wisp, tape: Tape): void {
  console.log("v08", tape.v08.byteLength)
  const v08ptr = ctx.api.wisp_heap_load_v08(ctx.heap, tape.v08.byteLength)
  const v32ptr = ctx.api.wisp_heap_load_v32(ctx.heap, tape.v32.byteLength / 4)

  transfer(ctx, tape.v08, v08ptr)
  transfer(ctx, tape.v32, v32ptr)
  
  for (const [tag, cols] of Object.entries(tabs)) {
    let j = 0
    for (const col of cols) {
      const buf = tape.tab[tag][col]
      const len = buf.byteLength

      const ptr = ctx.api.wisp_heap_load_tab_col(
        ctx.heap, ctx.tag[tag], j++, len / 4
      )

      if (len > 0 && !ptr) {
        throw new Error("wisp_heap_load_tab_col")
      }

      transfer(ctx, buf, ptr)
    }
  }  
}

export function make(ctx: Wisp, book: Book): Tape {
  const tab: Tab = {}

  const datptr = ctx.api.wisp_dat_init(ctx.heap)
  const buf = ctx.api.memory.buffer
  const dat = new DataView(buf, datptr)

  ctx.api.wisp_dat_read(ctx.heap, datptr)

  let bytes = 0
  
  let i = 0
  const next = () => dat.getUint32(4 * i++, true)
  for (const [tag, cols] of Object.entries(tabs)) {
    tab[tag] = {}
    const n = next()
    for (const col of cols) {
      const ptr = next()
      tab[tag][col] = buf.slice(ptr, ptr + 4 * n)
      bytes += 4 * n
    }
  }

  const tape = {
    book,
    tab,
    v08: ctx.v08slice(),
    v32: ctx.v32slice(),
  }

  bytes += tape.v08.byteLength + tape.v32.byteLength
  
  console.info(`wisp: saved ${bytes / 1024} KB`)
  
  return tape
}
