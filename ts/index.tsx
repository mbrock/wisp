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

import { Wisp, View, WispAPI } from "./wisp"
import { WASI } from "./wasi"

import * as ReactDOM from "react-dom"
import * as React from "react"

const buttonStyle = {
  border: "1.5px outset #ddd",
  background: "#f0f0f0",
  fontSize: "inherit",
  flexGrow: 1,
}

function renderWay(data: View, way: number, child: JSX.Element): JSX.Element {
  if (way == data.ctx.sys.top)
    return child
  
  const { hop, fun, acc, arg } = data.row("ktx", way)

  const accs = listItems(data, acc)
  const args = listItems(data, arg)

  function show(x: number, i: number): JSX.Element {
    return <Val data={data} v={x} key={i} />
  }
  
  const me = (
    <div className="list">
      {show(fun, 0)}
      {accs.map(show)}
      {child}
      {args.map(show)}
    </div>
  )

  if (hop == data.ctx.sys.top) {
    return me
  } else {
    return renderWay(data, hop, me)
  }
}

const Debugger = ({ data, run }: { data: View, run: number }) => {
  function doStep() {
    ctx.api.wisp_eval_step(ctx.heap, run)
    render()
  }

  const row = data.row("run", run)
  const { way, exp, val, env } = row
  
  const cur = exp == data.ctx.sys.nah ? val : exp

  const disabled =
    exp == data.ctx.sys.nah &&
    way == data.ctx.sys.top &&
    env == data.ctx.sys.nil

  const color = exp == data.ctx.sys.nah ? "#aaf5" : "#ffa5"
  
  return (
    <div style={{
      border: "1px solid #ccc",
      background: "#fffa",
      padding: 5,
      gap: 5,
      display: "flex",
      flexDirection: "column",
    }}>
      <header style={{           
        display: "flex",
        justifyContent: "space-between",
      }}>
        <button disabled={disabled} onClick={doStep} style={buttonStyle}>
          Step
        </button>
      </header>
      <div>
        {renderWay(data, way,
          <div style={{ background: color, borderRadius: 6, padding: "0 2.5px" }}>
            <Val data={data} v={cur} />
          </div>)}
      </div>
      {env == data.ctx.sys.nil ? null :
        <Val data={data} v={env} />}
    </div>
  )
}

function table(data: View, row: Record<string, number>) {
  const tableStyle = {
    padding: "5px 10px",
  }
  
  return (
    <table style={tableStyle}>
      <tbody>
        {
          Object.keys(row).map((k, i) =>
            <tr key={i}>
              <td>{k}</td>
              <td><Val data={data} v={row[k]} /></td>
            </tr>
          )
        }
      </tbody>
    </table>
  )
}

function listItems(data: View, v: number): number[] {
  const list = []
  let cur = v
  while (cur != data.ctx.sys.nil) {
    const { car, cdr } = data.row("duo", cur)
    list.push(car)
    if (data.ctx.tagOf(cdr) == "duo") {
      cur = cdr
    } else if (cdr == data.ctx.sys.nil) {
      break
    } else {
      list.push(cdr)
      break
    }
  }

  return list
}

const Val = ({ data, v }: { data: View, v: number }) => {
  const vtag = data.ctx.tagOf(v)
  switch (vtag) {
    case "int":
      return <span>{v}</span>

    case "jet": {
      const name = data.jetName(v)
      return <span>{name}</span>
    }

    case "sys": {
      if (v === data.ctx.sys.nil) {
        return <div className="list nil"></div>
      } else if (v === data.ctx.sys.t) {
        return <span>T</span>
      } else if (v === data.ctx.sys.top) {
        return <span>TOP</span>
      } else if (v === data.ctx.sys.nah) {
        return <span>NAH</span>
      } else {
        return <span>{v}</span>
      }
    }

    case "v08": {
      const str = data.str(v)
      return <div className="string">"{str}"</div>
    }

    case "v32": {
      const v32 = Array.from(data.getV32(v))
      return <div className="list v32">
        {v32.map((x, i) => <Val data={data} v={x} key={i} />)}
      </div>
    }

    case "duo": {
      const list = []
      let dotted = false
      let cur = v
      while (cur != data.ctx.sys.nil) {
        const { car, cdr } = data.row("duo", cur)
        list.push(car)
        if (data.ctx.tagOf(cdr) == "duo") {
          cur = cdr
        } else if (cdr == data.ctx.sys.nil) {
          break
        } else {
          list.push(cdr)
          dotted = true
          break
        }
      }

      return (
        <div className={`list ${dotted ? "dotted" : ""}`}>
          {
            list.map((x, i) =>
              <Val data={data} v={x} key={i} />)
          }
        </div>
      )
    }

    case "fun": {
      const row = data.row("fun", v)
      if (row.sym != data.ctx.sys.nil)
        return <Val data={data} v={row.sym} />
      else
        return table(data, data.row("fun", v))
    }

    case "run": {
      return <Debugger data={data} run={v} /> 
    }

    case "ktx": {
      return table(data, data.row("ktx", v))
    }

    case "sym": {
      const { str, pkg, fun } = data.row("sym", v)
      const funtag = data.ctx.tagOf(fun)
      const symstr = data.str(str)
      const pkgstr = pkg == data.ctx.sys.nil
        ? "#" : data.str(data.row("pkg", pkg).nam)

      return (
        <span className="sym"
              data-package={pkgstr}
              data-name={symstr}
              data-funtag={funtag} >
          <span className="pkg">{pkgstr}:</span>
          <span className="str">{data.str(str)}</span>
        </span>
      )
    }

    default:
      return <span style={{ color: "red" }}>{`${vtag}: ${v}`}</span>
  }
}

const Line = ({ data, turn, i }: {
  data: View,
  turn: Turn,
  i: number,
}) => {
  return (
    <div style={{ marginBottom: "0.5rem",
                  display: "flex",
                  alignItems: "start",
                  flexDirection: "column",
                  gap: "5px",
                }}>
      <span>
        <span style={{ opacity: 0.4, padding: "0 1.5rem 0 0" }}>Exp #{i}</span>
        <Val data={data} v={turn.exp} />
      </span>
      <span style={{ display: "flex" }}>
        <span style={{ opacity: 0.4, padding: "0 1.5rem 0 0" }}>Val #{i}</span>
        <Val data={data} v={turn.val} />
      </span>
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

  function exec(s: string) {
    const exp = ctx.read(s)
    const val = ctx.eval(exp)
    setLines(xs => [...xs, {
      exp, val
    }])    
  }

  React.useEffect(() => {
    exec("(run '(let ((x (+ 1 2)) (y 3)) (+ x y)))")
    exec("(run '(append '(1 2 3) '(a b c)))")
  }, [])

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
              <Line data={ctx.view()} turn={turn} i={i} key={i} />
          )
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

let ctx: Wisp = null

function render() {
  ReactDOM.render(
    <Home ctx={ctx} />,
    document.querySelector("#app")
  )
}

onload = async () => {
  const wasi = new WASI
  const { instance } = await WebAssembly.instantiateStreaming(
    fetch(window.wispWasmUrl), {
    wasi_snapshot_preview1: wasi.exports()
  })

  const exports = instance.exports as unknown as WispAPI
  
  wasi.setMemory(exports.memory)

  ctx = new Wisp(instance)

  render()
}
