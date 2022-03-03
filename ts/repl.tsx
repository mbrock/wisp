import * as fs from "fs/promises"

import * as wisp from "./wisp"

import * as React from "react"

import { render, Text, Box, Spacer } from "ink"

import TextInput from "ink-text-input"
import useStdoutDimensions from "ink-use-stdout-dimensions"

const Repl = ({ ctx, data }: { ctx: wisp.Wisp, data: wisp.CtxData }) => {
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
          <TextInput value={input} onChange={setInput} onSubmit={submit} />
        </Box>
      </Box>
      <Box>
        <Text>{data.tab.duo.car.length} duos</Text>
      </Box>
    </Box>
  )
}

async function main() {
  const wasmCode = await fs.readFile("zig-out/lib/wisp.wasm")
  const ctx = new wisp.Wisp(await WebAssembly.instantiate(wasmCode))

  render(<Repl ctx={ctx} data={ctx.readData()}/>)
}

main()
