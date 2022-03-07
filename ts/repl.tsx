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

import * as fs from "fs/promises"

import * as wisp from "./wisp"

import * as React from "react"

import { render, Text, Box } from "ink"

import TextInput from "ink-text-input"
import useStdoutDimensions from "ink-use-stdout-dimensions"

const Repl = ({ ctx, data }: {
  ctx: wisp.Wisp,
  data: wisp.View
}) => {
  const [input, setInput] = React.useState("")
  const [outputs, setOutputs] = React.useState([])
  const [tw, th] = useStdoutDimensions()

  function submit(line: string) {
    setInput("")
    const exp = ctx.read(line)
    const val = ctx.eval(exp)
    setOutputs(xs => [...xs, { exp, val }])
  }

  return (
    <Box flexDirection="row" width={tw} height={th}>
      <Box flexDirection="column" flexGrow={1}>
        <Box flexDirection="column" flexGrow={1}>
          {
            outputs.map((output, i) => (
              <Box key={i}>
                <Text dimColor>{output.exp}</Text>
                <Text bold>{" ↦ "}</Text>
                <Text>{output.val}</Text>
              </Box>
            ))
          }
        </Box>
        <Box>
          <Box paddingRight={1}>
            <Text dimColor >{">"}</Text>
          </Box>
          <TextInput
             value={input}
             onChange={setInput}
             onSubmit={submit} />
        </Box>
      </Box>
      <Box>
        <Text>{data.tab.duo.car.length} duos</Text>
      </Box>
    </Box>
  )
}

async function main() {
  const source = await fs.readFile("zig-out/lib/wisp.wasm")
  const instance = await WebAssembly.instantiate(source)
  const ctx = new wisp.Wisp(instance)

  render(<Repl ctx={ctx} data={ctx.view()}/>)
}

main()
