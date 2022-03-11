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

import * as ReactDOM from "react-dom"
import * as React from "react"

import "./index.css"

import wispWasmPath from "../zig-out/lib/wisp.wasm"

import { Wisp, View, WispAPI } from "./wisp"
import { WASI } from "./wasi"

import { Editor } from "./edit"

import {
  VscDebugStepInto, VscDebugStepOut, VscDebugStepOver, VscGithub
} from "react-icons/vsc"

const css = {
  sexp: {
    list: `border-x-2 dark:border-x dark:rounded-md border-gray-400 dark:border-neutral-400 rounded-lg px-1 inline-flex flex-row flex-wrap gap-x-2 items-center hover:border-cyan-600 dark:hover:border-amber-600 cursor-pointer w-fit max-w-sm`,
    v32: `border-x-4 border-gray-400 dark:border-neutral-600 border-y rounded-md px-1 py-1 inline-flex flex-row flex-wrap gap-x-2 items-center hover:border-cyan-600 dark:hover:border-amber-600 cursor-pointer w-fit max-w-sm`,
  }
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
    <div className={css.sexp.list}>
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

const RestartButton: React.FC<{
  action: () => void,
}> = ({ action, children }) => {
  return (
    <button onClick={action}
      className="border rounded dark:border-neutral-500 dark:bg-neutral-700 text-sm">
      {children}
    </button>
  )
}

const Fetch = ({ data, run, ask }: {
  data: View, run: number, ask: number[]
}) => {
  const [url] = listItems(data, ask[2])
  const urlstr = data.str(url)

  async function doFetch() {
    const x = await fetch(urlstr).then(x => x.text())
    const txtstr = data.ctx.newstr(x)
    const txtv08 = data.api.wisp_heap_v08_new(ctx.heap, txtstr, x.length)
    data.ctx.api.wisp_run_restart(ctx.heap, run, txtv08)
    render()
  }
  
  return (
    <div className="flex flex-col gap-1">
      <RestartButton action={doFetch}>
        Download <Val data={data} v={url} />
      </RestartButton>
      <RestartButton action={() => {}}>
        Abort evaluation
      </RestartButton>
    </div>
  )
}

const Restarts = ({ data, run }: { data: View, run: number }) => {
  function restart(s: string): void {
    const src = ctx.read(s)
    data.ctx.api.wisp_run_restart(ctx.heap, run, src)
    render()
  }

  const row = data.row("run", run)
  const { err } = row

  const v32 = Array.from(data.getV32(err))

  const errsym = data.row("sym", v32[0])
  const errstr = data.str(errsym.str)

  const isRequest = errstr === "REQUEST"
  const asksym = data.row("sym", v32[1]) 
  const askstr = data.str(asksym.str)
  const isFetch = askstr === "FETCH"

  return (
    <div className="flex flex-col gap-1 mb-1">
      <div className="flex flex-col gap-1">
        { isFetch ? <Fetch data={data} run={run} ask={v32} /> : null }
         
        <Form done={restart} placeholder="Provide another value" />
      </div>
    </div>
  )
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

  let color = "ring-1 ring-gray-300 dark:ring-amber-600 py-px px-1 rounded-sm "

  if (err != data.ctx.sys.nil) {
    color += "bg-red-100 dark:bg-red-600/50"
  } else if (exp == data.ctx.sys.nah) {
    color += "bg-blue-100 dark:bg-green-600/50"
  } else {
    color += "bg-yellow-50 dark:bg-amber-600/50"
  }

  const IconButton: React.FC<{
    action: () => void,
    left?: boolean,
    right?: boolean,
    disabled?: boolean,
  }> = ({
    action, left, right, disabled, children,
  }) => {
    const classes = `
      inline-flex items-center py-1 px-2 border border-gray-300 dark:border-neutral-600
      bg-white dark:bg-neutral-700
      text-gray-700 dark:text-neutral-300
      hover:bg-gray-50 dark:hover:bg-neutral-500
      font-medium
      focus:ring-1 focus:ring-indigo-500
      ${left ? "rounded-l-md" : (right ? "rounded-r-md" : "")}
      ${disabled ? "text-gray-400 dark:text-neutral-500" : ""}
    `
    
    return (
      <button className={classes}
        disabled={disabled}
        onClick={action}>
        {children}
      </button>
    )
  }

  const envs: number[][][] = listItems(data, env).map((env, i) => {
    const v32 = Array.from(data.getV32(env))
    const vars = []
    
    while (v32.length > 0) {
      vars.push([v32.shift(), v32.shift()])
    }
    
    return vars
  })

  const renderRow = ([k, v]: number[], i: number) => (
    <tr key={i}>
      <td className="px-1 font-medium text-right"><Val data={data} v={k}/></td>
      <td className="px-1 text-left"><Val data={data} v={v}/></td>
    </tr>
  )

  const renderScope = (scope: number[][], i: number) => (
    <tbody key={i}>
      {scope.map(renderRow)}
    </tbody>
  )
  
  const scopes = envs.length > 0 ? (
    <table className="table-auto divide-y dark:divide-neutral-600 bg-white dark:bg-neutral-800 dark:text-neutral-100
                      border mb-1 text-sm">
      {envs.map(renderScope)}
    </table>
  ) : null

  const condition = (
    err === data.ctx.sys.nil
     ? <></>
     : <Val data={data} v={err} style="mb-1 bg-red-700/30" />
  )

  const restarts = (
    err === data.ctx.sys.nil
     ? <></>
     : <Restarts data={data} run={run} />
  )

  return (
    <div className="border-gray-300 dark:border-neutral-600 bg-white dark:bg-neutral-800 dark:text-neutral-50 border rounded-lg flex flex-row gap-2 divide-x-2 dark:divide-neutral-600 mb-1">
      <div className="flex flex-grow flex-col gap-1 p-1 px-2">
         <span className="font-medium text-sm text-gray-500 dark:text-neutral-400">Evaluation</span>
         {renderWay(data, way,
            <Val data={data} v={cur} style={color} />
          )}
      </div>
  
      <aside className="bg-gray-50 dark:bg-neutral-800 flex flex-col divide-y dark:divide-neutral-600 rounded-r-lg">
        <div className="flex p-1">
          <IconButton action={doStep} left>
            <VscDebugStepInto />
          </IconButton>
          <IconButton action={() => alert("not implemented")} disabled>
            <VscDebugStepOver />
          </IconButton>
          <IconButton action={() => alert("not implemented")} right disabled>
            <VscDebugStepOut />
          </IconButton>
        </div>
        <div className="flex flex-col gap px-1">
          <span className="font-medium text-sm text-gray-500">
            Scopes
          </span>
          {scopes}
        </div>
        <div className="px-1 flex flex-col">
          <span className="font-medium text-sm text-gray-500">
            Condition
          </span>
          {condition}
        </div>
        <div className="px-1 flex flex-col">
          <span className="font-medium text-sm text-gray-500">
            Restarts
          </span>
          {restarts}
        </div>
      </aside>

    </div>
  )
}

function table(data: View, row: Record<string, number>) {
  return (
    <table>
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

const Val = ({ data, v, style }: { data: View, v: number, style?: string }) => {
  const vtag = data.ctx.tagOf(v)
  switch (vtag) {
    case "int": {
      const i = v & (1 << 30) ? ((-(~v << 1) >> 1) - 1): v
      return <span className={`dark:text-neutral-300 ${style}`}>{i}</span>
    }

    case "jet": {
      const name = data.jetName(v)
      return symbolSpan("WISP", name, "jet", style)
    }

    case "sys": {
      if (v === data.ctx.sys.nil) {
        return symbolSpan("WISP", "NIL", "nil", style)
      } else if (v === data.ctx.sys.t) {
        return symbolSpan("WISP", "T", "nil", style)
      } else if (v === data.ctx.sys.top) {
        return symbolSpan("WISP", "TOP", "nil", style)
      } else if (v === data.ctx.sys.nah) {
        return symbolSpan("WISP", "NAH", "nil", style)
      } else {
        return <span className={style}>{v}</span>
      }
    }

    case "v08": {
      const str = data.str(v)
      return <span className={`font-mono text-green-800 dark:text-green-200 ${style}`}>"{str}"</span>
    }

    case "v32": {
      const v32 = Array.from(data.getV32(v))
      return <span className={`${css.sexp.v32} ${style}`}>
        {v32.map((x, i) => <Val data={data} v={x} key={i} />)}
      </span>
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
        <span className={`${css.sexp.list} ${style}`}>
          {
            list.map((x, i) =>
              <Val data={data} v={x} key={i} />)
          }
        </span>
      )
    }

    case "fun": {
      const row = data.row("fun", v)
      if (row.sym != data.ctx.sys.nil)
        return <Val data={data} v={row.sym} style={style} />
      else
        return (
          <span className={`${css.sexp.list} ${style}`}>
            {symbolSpan("WISP", "FN", "jet")}
            <Val data={data} v={row.par} />
            <Val data={data} v={row.exp} />
          </span>
        )
    }

    case "run": {
      return (
        <Debugger data={data} run={v} />
     )
    }

    case "ktx": {
      return (
        <div className={style}>
          {table(data, data.row("ktx", v))}
        </div>
      )
    }

    case "sym": {
      const { str, pkg, fun } = data.row("sym", v)
      const funtag = data.ctx.tagOf(fun)
      const symstr = data.str(str)
      const pkgstr = pkg == data.ctx.sys.nil
        ? "#" : data.str(data.row("pkg", pkg).nam)

      return symbolSpan(pkgstr, symstr, funtag, style)
    }

    default:
      return <span className={style}>{`${vtag}: ${v}`}</span>
  }
}

function symbolSpan(
  pkgstr: string, symstr: string, funtag: string, style?: string
) {
  const extra = extraStyleForSymbol(funtag)

  const classes = `
    sym lowercase tracking-tighter text-gray-800 hover:text-blue-600 dark:hover:text-amber-600
    dark:text-neutral-300
    ${extra}
    ${style || ""}
  `

  return (
    <span className={classes}
          data-package={pkgstr}
          data-name={symstr}
          data-funtag={funtag} >
      <span className="pkg">{pkgstr}:</span>
      <span className="str">{symstr}</span>
    </span>
  )
}

function extraStyleForSymbol(funtag: string): string {
  switch (funtag) {
    case "jet": return "font-medium"
    case "fun": return "text-slate-600 dark:text-slate-300"
    case "mac": return "text-cyan-600 dark:text-cyan-300"
  }

  return ""
}

const Line = ({ data, turn, i }: {
  data: View,
  turn: Turn,
  i: number,
}) => {
  const run = data.row("run", turn.run) as unknown as Run

  return (
    <div className="block p-2 px-3 flex flex-col justify-between gap-2 lg:flex-row items-baseline">
      <Val data={data} v={turn.src} style="" />
      { run.err == data.ctx.sys.nil
          ? <Val data={data} v={run.val} />
          : <Err data={data} run={turn.run} /> }
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

const Form = ({ done, placeholder, autoFocus }: {
  done: (x: string) => void,
  placeholder?: string,
  autoFocus?: bool,
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
    <form onSubmit={onSubmit} >
      <input type="text"
        className="w-full bg-white dark:bg-neutral-800 dark:text-neutral-100 py-0 focus:ring-0 focus:outline-0 focus:border-gray-600 border rounded"
        autoFocus={autoFocus} autoComplete="off"
        placeholder={placeholder}
        value={history[historyCursor]}
        onKeyDown={onKeyDown}
        onChange={onChange} />
    </form>
  )
}

const Home = ({ ctx, data }: { ctx: Wisp, data: View }) => {
  const [turns, setTurns] = React.useState([] as Turn[])

  function exec(s: string) {
    const src = ctx.read(s)
    const run = ctx.api.wisp_run_init(ctx.heap, src)

    ctx.api.wisp_run_eval(ctx.heap, run, 10_000)

    render()

    setTurns(xs => [...xs, { src, run }])
  }

  React.useEffect(() => {
    exec("(run '(append (list 1 x 3) '(a b c)))")
    exec("(run '(mapcar (lambda (x) (+ x 1)) '(1 2 3)))")
    exec("(+ 1 2 3)")
    exec("(run '(request 'fetch \"https://httpbin.org/uuid\"))");
  }, [])

  return (
    <div className="absolute inset-0 flex flex-col bg-gray-100 dark:bg-neutral-900">
      <Titlebar />
      <Notebook data={data} turns={turns} />
      <Editor exec={exec} />
    </div>
  )
}

const Notebook = ({ data, turns }: {
  data: View,
  turns: Turn[],
}) => {
  const classes =
    "divide-y-2 dark:divide-neutral-600 flex flex-col flex-grow overflow-y-auto"

  const ref = React.useRef<HTMLUListElement>()
  
  React.useEffect(() => {
    if (ref.current) {
      ref.current.scrollTop = ref.current.scrollHeight
    }
  }, [turns.length]);
  
  const lines = turns.map((turn, i) =>
    <ol key={i}>
      <Line data={data} turn={turn} i={i + 1}/>
    </ol>
  )
  
  return (
    <ul role="list" ref={ref} className={classes}>
      {lines}
    </ul>
  )
}

const Titlebar = () => {
  return (
    <header className="flex justify-between border-b-2 px-3 py-1 bg-slate-50 dark:bg-stone-900 dark:border-neutral-600">
      <span className="font-semibold tracking-tight text-gray-800 dark:text-neutral-400 flex gap-1">
        Wisp
        <span className="text-gray-500">v0.5</span>
      </span>
      <a href="https://github.com/mbrock/wisp"
         className="tracking-tight text-blue-800 dark:text-blue-200 flex column items-center"
         target="_blank">
        <VscGithub />
      </a>
    </header>
  )
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
    fetch(wispWasmPath), {
    wasi_snapshot_preview1: wasi.exports()
  })

  const exports = instance.exports as unknown as WispAPI

  wasi.setMemory(exports.memory)

  ctx = new Wisp(instance)

  render()
}
