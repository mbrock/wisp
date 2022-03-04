import * as ReactDOM from "react-dom"
import * as React from "react"

import { Wisp, View } from "./wisp"

const Val = ({ data, val }: { data: View, val: number }) => {
  switch (data.ctx.tagOf(val)) {
    case "int":
      return <span>{val}</span>

    case "sys": {
      if (val === data.ctx.sys.nil) {
        return <span>NIL</span>
      } else if (val === data.ctx.sys.t) {
        return <span>T</span>
      }
    }

    case "duo": {
      const { car, cdr } = data.row("duo", val)
      return (
        <div className="list">
          <Val data={data} val={car} />
          <Val data={data} val={cdr} />
        </div>
      )
    }

    case "sym": {
      const { str } = data.row("sym", val)
      return (
        <span className="sym">
          {data.str(str)}
        </span>
      )
    }

    default:
      return <span style={{ color: "red" }}>{val}</span>
  }
}

const Line = ({ data, turn }: { data: View, turn: Turn }) => {
  return (
    <div>
      <Val data={data} val={turn.exp} />
      ↦
      <Val data={data} val={turn.val} />
    </div>
  )
}

interface Turn {
  exp: number
  val: number
}

const Home = ({ ctx }: { ctx: Wisp }) => {
  const [lines, setLines] = React.useState([] as Turn[])
  const [input, setInput] = React.useState("")

  return (
    <div id="repl">
      <header className="titlebar">
        <span>
          <b>Notebook</b>
        </span>
        <span>
          Package: <em>WISP</em>
        </span>
      </header>
      <div id="output">
        {
          lines.map(
            (turn, i) =>
              <Line data={ctx.view()} turn={turn} key={i} />)
        }
      </div>
      <form id="form" 
        onSubmit={e => {
            e.preventDefault()
            const exp = ctx.read(input)
            const val = ctx.eval(exp)
            setLines(xs => [...xs, {
              exp, val
            }])
            setInput("")
            return false
        }}>
        <input
          id="input" autoFocus autoComplete="off"
          value={input}
          onChange={e => setInput(e.target.value)}
        />
      </form>
    </div>
  )
}

declare global {
  interface Window {
    wispWasmUrl: string
  }
}

onload = async () => {
  const ctx = new Wisp(
    await WebAssembly.instantiateStreaming(
      fetch(window.wispWasmUrl),
      {}))

  ReactDOM.render(
    <Home ctx={ctx} />,
    document.querySelector("#app")
  )
}
