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
      {accs.reverse().map(show)}
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
  const { way, exp, val, env, err } = row

  const cur = exp == data.ctx.sys.nah ? val : exp

  const disabled =
    err != data.ctx.sys.nil || (
      exp == data.ctx.sys.nah
      && way == data.ctx.sys.top
      && env == data.ctx.sys.nil
    )

  let color = "#ffa5"

  if (err != data.ctx.sys.nil)
    color = "#faa5"
  else if (exp == data.ctx.sys.nah)
    color = "#aaf5"

  function restart(s: string): void {
    const src = ctx.read(s)
    data.ctx.api.wisp_run_restart(ctx.heap, run, src)
    render()
  }

  return (
    <div style={{
      border: "1px solid #ccc",
      background: "#fffa",
      padding: 5,
      gap: 5,
      display: "flex",
      flexDirection: "column",
      flexGrow: 1,
    }}>
      <header style={{
        display: "flex",
        justifyContent: "space-between",
      }}>
        <button disabled={disabled} onClick={doStep} style={buttonStyle}>
          Step
        </button>
      </header>
      {renderWay(data, way,
        <div style={{ background: color, borderRadius: 6, padding: "0 2.5px" }}>
          <Val data={data} v={cur} />
        </div>
      )}
      {env == data.ctx.sys.nil ? null :
        <Val data={data} v={env} />}
      {err == data.ctx.sys.nil ? null :
        <div>
          <Val data={data} v={err} />
          <Form done={restart} placeholder="Restart" />
        </div>
        }
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

const Err = ({ data, run }: { data: View, run: number }) => {
  return (
    <Debugger data={data} run={run} />
  )
}

const Val = ({ data, v }: { data: View, v: number }) => {
  const vtag = data.ctx.tagOf(v)
  switch (vtag) {
    case "int": {
      const i = v & (1 << 30) ? ((-(~v << 1) >> 1) - 1): v
      return <span>{i}</span>
    }

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
        return <span><Val data={data} v={row.sym} /></span>
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
  const run = data.row("run", turn.run) as unknown as Run

  const spanStyle: React.CSSProperties = {
    fontFamily: "var(--sans)",
    fontSize: "14px",
    color: "#444",
  }

  const divStyle: React.CSSProperties = {
    display: "flex",
    alignItems: "start",
    flexDirection: "column",
    alignContent: "center",
    gap: "5px",
    width: "100%",
  }
  
  return (
    <div style={divStyle}>
      <div style={divStyle}>
        <span style={spanStyle}>
          Expression:
        </span>
        <span style={{ marginLeft: 5 }}>
          <Val data={data} v={turn.src} />
        </span>
      </div>
      <div style={divStyle}>
        <span style={spanStyle}>
          Result:
        </span>
        <span style={{ display: "flex", marginLeft: 5, width: "100%" }}>
          { run.err == data.ctx.sys.nil
              ? <Val data={data} v={run.val} />
              : <Err data={data} run={turn.run} /> }
        </span>
      </div>
    </div>
  )
}

interface Run {
  exp: number
  val: number
  err: number
  way: number
  env: number
}

interface Turn {
  src: number
  run: number
}

const Form = ({ done, placeholder }: {
  done: (x: string) => void,
  placeholder?: string,
}) => {
  const [history, setHistory] = React.useState([""])
  const [historyCursor, setHistoryCursor] = React.useState(0)

  function onSubmit(e: React.FormEvent) {
    e.preventDefault()

    done(history[historyCursor])

    if (historyCursor > 0) {
      setHistory(xs => ["", history[historyCursor], ...xs])
    } else {
      setHistory(xs => ["", ...xs])
    }

    return false
  }

  function onKeyDown(e: React.KeyboardEvent<HTMLInputElement>) {
    if (e.key === "ArrowUp") {
      e.preventDefault()
      if (historyCursor + 1 < history.length) {
        setHistoryCursor(historyCursor + 1)
      }
    } else if (e.key === "ArrowDown") {
      e.preventDefault()
      if (historyCursor > 0) {
        setHistoryCursor(historyCursor - 1)
      }
    }
  }

  function onChange(e: React.ChangeEvent<HTMLInputElement>) {
    setHistory(xs => [e.target.value, ...xs.slice(1)])
    setHistoryCursor(0)
  }

  return (
    <form onSubmit={onSubmit}>
      <input autoFocus autoComplete="off"
        placeholder={placeholder}
        value={history[historyCursor]}
        onKeyDown={onKeyDown}
        onChange={onChange} />
    </form>
  )
}

const Home = ({ ctx, data }: { ctx: Wisp, data: View }) => {
  const [lines, setLines] = React.useState([] as Turn[])

  function exec(s: string) {
    const src = ctx.read(s)
    const run = ctx.api.wisp_run_init(ctx.heap, src)

    ctx.api.wisp_run_eval(ctx.heap, run, 10_000)

    render()

    setLines(xs => [...xs, { src, run }])
  }

  React.useEffect(() => {
    exec("(run '(append (list 1 x 3) '(a b c)))")
    exec("(run '(mapcar (lambda (x) (+ x 1)) '(1 2 3)))")
  }, [])

  return (
    <div id="repl">
      <header className="titlebar">
        <span>
          <b>Wisp Notebook</b>
        </span>
        <span>
          <a href="https://github.com/mbrock/wisp"
             target="_blank"
             style={{ opacity: 0.7 }}>
            mbrock/wisp
          </a>
        </span>
      </header>
      <div id="output">
        {
          lines.map(
            (turn, i) =>
              <Line data={data} turn={turn} i={i} key={i} />
          )
        }
      </div>
      <Form done={exec} placeholder="Wisp expression" />
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
  const data = ctx.view()

  ReactDOM.render(
    <Home ctx={ctx} data={data} />,
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
