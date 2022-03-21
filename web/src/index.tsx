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

import wispPackageJSON from "../package.json"

import zustand from "zustand"
import zustandContext from "zustand/context"

import "./index.css"
import "../vendor/inter/inter.css"

import wispWasmPath from "wisp.wasm"

import { WASI } from "./wasi"
import { Wisp, WispAPI, Data, tagOf, getRow, getUtf8String, getV32 } from "./wisp"
import * as Tape from "./tape"

import { Editor } from "./edit"

import {
  VscDebugStepInto, VscDebugStepOut, VscDebugStepOver, VscFolderOpened, VscGithub, VscSave
} from "react-icons/vsc"

import { IoAddCircleOutline } from "react-icons/io5"

import ShortUniqueId from "short-unique-id"
import { IconButton } from "./button"
import { Auth0Provider, useAuth0 } from "@auth0/auth0-react"

const initialDocument = `(cons 'ok
      (handle 'tag
              (fn () (* 2 (send! 'tag 5)))
              (fn (v k)
                (list (list 'continuation k)
                      (list 'result (call k (+ 1 v)))))))`

const uuid = new ShortUniqueId({ length: 8 })

export type Note = {
  key: string,
  run?: number,
  editorState?: any,
}

interface Store {
  ctx: Wisp
  data: Data
  tape: number
  notes: Record<string, Note>
  noteOrder: string[]

  refresh(): Data
  newEmptyNote(): void
  loadNotes(notes: [Note]): void
  setNoteRun(key: string, run: number): void
  setNoteEditorState(key: string, editorState: any): void
}

const initialNoteKey = uuid()

const { Provider, useStore } = zustandContext<Store>()

const createStore = (ctx: Wisp) => () => zustand<Store>(set => ({
  ctx,
  data: ctx.loadData(),
  tape: 0,

  noteOrder: [initialNoteKey],
  notes: {
    [initialNoteKey]: {
      key: initialNoteKey,
      editorState: {
        doc: initialDocument,
      },
    }
  },

  setCtx(ctx: Wisp) {
    set(() => ({ ctx }))
  },

  refresh() {
    let data

    set(state => {
      data = state.ctx.loadData()
      console.log({ data })
      return { data }
    })

    return data
  },

  newEmptyNote() {
    const key = uuid()
    console.log(`new ${key}`)
    set(state => ({
      noteOrder: [...state.noteOrder, key],
      notes: {
        ...state.notes,
        [key]: { key, src: "" }
      },
    }))
  },

  loadNotes(noteList: [Note]) {
    const noteOrder = noteList.map((x: Note) => x.key)
    const notes = {}
    for (let note of noteList) {
      notes[note.key] = note
    }

    set(state => ({
      tape: state.tape + 1,
      noteOrder,
      notes,
      data: state.ctx.loadData(),
    }))
  },

  setNoteRun(key: string, run: number) {
    set(state => ({
      notes: {
        ...state.notes,
        [key]: {
          ...state.notes[key],
          run,
        }
      }
    }))
  },

  setNoteEditorState(key: string, editorState: any) {
    set(state => ({
      notes: {
        ...state.notes,
        [key]: {
          ...state.notes[key],
          editorState,
        }
      }
    }))
  },
}))

const css = {
  sexp: {
    list: `border-x-2 dark:border-x dark:rounded-md border-gray-400 dark:border-neutral-400 rounded-lg px-1 inline-flex flex-row flex-wrap gap-x-2 items-center hover:border-cyan-600 dark:hover:border-amber-600 cursor-pointer w-fit max-w-sm`,
    v32: `border-x-4 border-gray-400 dark:border-neutral-600 border-y rounded-md px-1 py-1 inline-flex flex-row flex-wrap gap-x-2 items-center hover:border-cyan-600 dark:hover:border-amber-600 cursor-pointer w-fit max-w-sm`,
  }
}

const Way: React.FC<{ way: number }> = ({ way, children }) => {
  const { data } = useStore()

  if (way == data.sys.top)
    return <>{children}</>

  function show(x: number, key?: React.Key): JSX.Element {
    return <Val v={x} key={key} />
  }

  if (tagOf(data, way) == "ktx") {
    const { hop, fun, acc, arg } = getRow(data, "ktx", way)

    const me = () => {
      if (fun === data.kwd.PROMPT) {
        return (
          <div className={css.sexp.list}>
            {show(fun)}
            {show(acc)}
            {children}
            {show(arg)}
          </div>
        )
      } else if (fun === data.kwd.LET) {
        // LET continuations have a peculiar representation
        // that's efficient to build up but slightly annoying to
        // parse for display.
        //
        // Take this example, where evaluation is now at <E2>.
        //
        //       (LET ((A E1) (B <E2>) (C E3) BODY)
        //
        // The continuation will look like this:
        //
        //   acc: (BODY A V1 B)     [except backwards]
        //   exp: E2
        //   arg: ((C E3))
        //
        // So the accumulator has the body, the finished
        // bindings, and the symbol for the binding which we are
        // evaluating, all in the same list.

        const items = listItems(data, acc).reverse()
        const body = show(items[0], 0)
        const bindings = []

        let i = 1

        // Render all the finished bindings.
        while (i < items.length - 1) {
          bindings.push(
            <div className={css.sexp.list} key={i}>
              {show(items[i])}
              {show(items[i + 1])}
            </div>
          )

          i += 2
        }

        // Render the current binding.
        bindings.push(
          <div className={css.sexp.list} key={i++}>
            {show(items[items.length - 1])}
            {children}
          </div>
        )

        // Render the upcoming bindings.
        for (const x of listItems(data, arg)) {
          bindings.push(show(x, i++))
        }

        return (
          <div className={css.sexp.list}>
            {show(fun)}
            <div className={css.sexp.list}>
              {bindings}
            </div>
            {body}
          </div>
        )

      } else {
        const accs = listItems(data, acc)
        const args = listItems(data, arg)

        return (
          <div className={css.sexp.list}>
            {show(fun)}
            {accs.reverse().map(show)}
            {children}
            {args.map(show)}
          </div>
        )
      }
    }

    if (hop == data.sys.top) {
      return me()
    } else {
      return <Way way={hop}>{me()}</Way>
    }
  } else {
    debugger
    throw new Error(tagOf(data, way))
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

const Fetch = ({ run, ask }: {
  run: number, ask: number[]
}) => {
  const { ctx, data, refresh } = useStore()

  const [url] = listItems(data, ask[2])
  const urlstr = getUtf8String(data, url)

  async function doFetch() {
    const x = await fetch(urlstr).then(x => x.text())
    const txtstr = ctx.newstr(x)
    const txtv08 = ctx.api.wisp_heap_v08_new(ctx.heap, txtstr, x.length)
    ctx.api.wisp_run_restart(ctx.heap, run, txtv08)
    refresh()
  }

  return (
    <div className="flex flex-col gap-1">
      <RestartButton action={doFetch}>
        Download <Val v={url} />
      </RestartButton>
      <RestartButton action={() => {}}>
        Abort evaluation
      </RestartButton>
    </div>
  )
}

const Restarts: React.FC<{ run: number }> = ({ run }) => {
  const { ctx, data, refresh } = useStore()

  function restart(s: string): void {
    const src = ctx.read(s)
    ctx.api.wisp_run_restart(ctx.heap, run, src)
    refresh()
  }

  const row = getRow(data, "run", run)
  const { err } = row

  const v32 = Array.from(getV32(data, err))

  const isFetch = () => {
    if (v32.length > 1) {
      const asksym = getRow(data, "sym", v32[1])
      const askstr = getUtf8String(data, asksym.str)
      return askstr === "FETCH"
    } else {
      return false
    }
  }

  return (
    <div className="flex flex-col gap-1 mb-1">
      <div className="flex flex-col gap-1">
        { isFetch() ? <Fetch run={run} ask={v32} /> : null }

        <Form done={restart} placeholder="Provide another value" />
      </div>
    </div>
  )
}

const Result = ({ val }: { val: number }) => {
  return (
    <div className="border-gray-300 dark:border-neutral-600 bg-white dark:bg-neutral-800 dark:text-neutral-50 border flex flex-row divide-x-2 dark:divide-neutral-600 w-full">
      <div className="flex flex-grow flex-col gap-1 p-1 px-2">
         <span className="font-medium text-sm text-gray-500 dark:text-neutral-400">Result</span>
         <Val v={val} />
      </div>
    </div>
  )
}

const Debugger: React.FC<{ run: number }> = ({ run }) => {
  const { data, ctx, refresh } = useStore()

  const doStep = (mode: "into" | "over" | | "out") => () => {
    const modeNumbers = { into: 0, over: 1, out: 2 }
    ctx.api.wisp_eval_step(ctx.heap, run, modeNumbers[mode])
    refresh()
  }

  const row = getRow(data, "run", run)
  const { way, exp, val, env, err } = row

  const cur = exp == data.sys.nah ? val : exp

  const disabled =
    err != data.sys.nil || (
      exp == data.sys.nah
      && way == data.sys.top
      && env == data.sys.nil
    )

  let color = "ring-1 ring-gray-300 dark:ring-amber-600 py-px px-1 font-mono "

  if (err != data.sys.nil) {
    color += "bg-red-100 dark:bg-red-600/50"
  } else if (exp == data.sys.nah) {
    color += "bg-blue-100 dark:bg-green-600/50"
  } else {
    color += "bg-yellow-50 dark:bg-amber-600/50"
  }

  function readEnv(env: number): number[][][] {
    return listItems(data, env).map(x => {
      const v32 = Array.from(getV32(data, x))
      const vars = []

      while (v32.length > 0) {
        vars.push([v32.shift(), v32.shift()])
      }

      return vars
    })
  }

  const renderRow = ([k, v]: number[], i: number) => (
    <tr key={i}>
      <td className="px-1 font-medium text-right">
        <Val v={k}/>
      </td>
      <td className="px-1 text-left">
        <Val v={v}/>
      </td>
    </tr>
  )

  const renderScope = (scope: number[][], i: number) => (
    <tbody key={i}>
      {scope.map(renderRow)}
    </tbody>
  )

  function renderScopes(env: number, key?: React.Key): React.ReactNode {
    const xs = readEnv(env)
    return xs.length > 0 ? (
      <table key={key} className="table-auto divide-y dark:divide-neutral-600 bg-white dark:bg-neutral-800 dark:text-neutral-100 border mb-1 text-sm">
        {xs.map(renderScope)}
      </table>
    ) : null
  }

  const lexicalScope = () => renderScopes(env)

  const dynamicScope = () => {
    const cache = new Set([env])
    const scopes = []
    let cur = way
    let key = 0

    function processEnv(env: number) {
      if (!cache.has(env)) {
        scopes.push(renderScopes(env, key++))
        cache.add(env)
      }
    }

    while (cur !== data.sys.top) {
      const ktx = getRow(data, "ktx", cur)
      processEnv(ktx.env)
      cur = ktx.hop
    }

    return <div>{scopes}</div>
  }

  const condition = (
    err === data.sys.nil
     ? <></>
     : <Val v={err} style="mb-1 bg-red-700/30" />
  )

  const restarts = (
    err === data.sys.nil
     ? <></>
     : <Restarts run={run} />
  )

  return (
    <div className="border border-gray-300 dark:border-neutral-600 bg-white dark:bg-neutral-800 dark:text-neutral-50 flex flex-row gap-2 divide-x-2 dark:divide-neutral-600 w-full">
      <div className="flex flex-grow flex-col gap-1 p-2">
        <Way way={way}>
          <Val v={cur} style={color} />
        </Way>
      </div>

      <aside className="bg-gray-50 dark:bg-neutral-800 flex flex-col divide-y dark:divide-neutral-600 rounded-r-lg">
        <div className="flex p-1">
          <IconButton action={doStep("into")} left>
            <VscDebugStepInto />
          </IconButton>
          <IconButton action={doStep("over")}>
            <VscDebugStepOver />
          </IconButton>
          <IconButton action={doStep("out")} right>
            <VscDebugStepOut />
          </IconButton>
        </div>
        <div className="flex flex-col gap px-1">
          <span className="font-medium text-sm text-gray-500">
            Lexical scope
          </span>
          {lexicalScope()}
        </div>
        <div className="flex flex-col gap px-1">
          <span className="font-medium text-sm text-gray-500">
            Dynamic scope
          </span>
          {dynamicScope()}
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

function listItems(data: Data, v: number): number[] {
  const list = []
  let cur = v
  while (cur != data.sys.nil) {
    const { car, cdr } = getRow(data, "duo", cur)
    list.push(car)
    if (tagOf(data, cdr) == "duo") {
      cur = cdr
    } else if (cdr == data.sys.nil) {
      break
    } else {
      list.push(cdr)
      break
    }
  }

  return list
}

const Val: React.FC<{ v: number, style?: string }> = ({ v, style }) => {
  const { data, ctx } = useStore()

  const vtag = tagOf(data, v)
  switch (vtag) {
    case "int": {
      const i = v & (1 << 30) ? ((-(~v << 1) >> 1) - 1): v
      return <span className={`dark:text-neutral-300 ${style}`}>{i}</span>
    }

    case "jet": {
      const name = ctx.jetName(v)
      return symbolSpan("WISP", name, "jet", style)
    }

    case "sys": {
      if (v === data.sys.nil) {
        return symbolSpan("WISP", "NIL", "nil", style)
      } else if (v === data.sys.t) {
        return symbolSpan("WISP", "T", "nil", style)
      } else if (v === data.sys.top) {
        return symbolSpan("WISP", "TOP", "nil", style)
      } else if (v === data.sys.nah) {
        return symbolSpan("WISP", "NAH", "nil", style)
      } else {
        return <span className={style}>{v}</span>
      }
    }

    case "v08": {
      const str = getUtf8String(data, v)
      return <span className={`font-mono text-green-800 dark:text-green-200 ${style}`}>"{str}"</span>
    }

    case "v32": {
      const v32 = Array.from(getV32(data, v))
      return <span className={`${css.sexp.v32} ${style}`}>
        {v32.map((x, i) => <Val v={x} key={i} />)}
      </span>
    }

    case "duo": {
      const list = []
      let dotted = false
      let cur = v
      while (cur != data.sys.nil) {
        const { car, cdr } = getRow(data, "duo", cur)
        list.push(car)
        if (tagOf(data, cdr) == "duo") {
          cur = cdr
        } else if (cdr == data.sys.nil) {
          break
        } else {
          list.push(cdr)
          dotted = true
          break
        }
      }

      return (
        <span className={`${css.sexp.list} ${style} ${dotted ? "dotted" : ""}`}>
          {
            list.map((x, i) =>
              <Val v={x} key={i} />)
          }
        </span>
      )
    }

    case "fun": {
      const row = getRow(data, "fun", v)
      if (row.sym != data.sys.nil)
        return <Val v={row.sym} style={style} />
      else
        return (
          <span className={`${css.sexp.list} ${style}`}>
            {symbolSpan("WISP", "FN", "jet")}
            <Val v={row.par} />
            <Val v={row.exp} />
          </span>
        )
    }

    case "mac": {
      const row = getRow(data, "mac", v)
      const detail = () => (
        <span className={`${css.sexp.list} ${style}`}>
          {symbolSpan("WISP", "MAC", "jet")}
          <Val v={row.par} />
          <Val v={row.exp} />
        </span>
      )

      return (
        <Expander detail={detail}>
          <Val v={row.sym} style={style} />
        </Expander>
      )
    }

    case "run":
      return <Debugger run={v} />

    case "ktx": {
      return (
        <span className={`${style} border p-1 border-cyan-400 rounded`}>
          <Way way={v}>⛳</Way>
        </span>
      )
    }

    case "sym": {
      const { str, pkg, fun } = getRow(data, "sym", v)
      const funtag = tagOf(data, fun)
      const symstr = getUtf8String(data, str)
      const pkgstr = pkg == data.sys.nil
        ? "#" : getUtf8String(data, getRow(data, "pkg", pkg).nam)

      return symbolSpan(pkgstr, symstr, funtag, style)
    }

    default:
      return <span className={style}>{`${vtag}: ${v}`}</span>
  }
}

const Expander: React.FC<{
  detail: () => React.ReactElement
}> = ({ detail, children }) => {
  let [show, setShow] = React.useState(false)

  return (
    <div onClick={() => setShow(x => !x)}>
      { show ? detail() : children }
    </div>
  )
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

interface Run {
  exp: number
  val: number
  err: number
  way: number
  env: number
}

const Form = ({ done, placeholder, autoFocus }: {
  done: (x: string) => void,
  placeholder?: string,
  autoFocus?: boolean,
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

  // React.useEffect(() => {
  //   exec("(run '(append (list 1 x 3) '(a b c)))")
  //   exec("(run '(map (fn (x) (+ x 1)) '(1 2 3)))")
  //   exec("(+ 1 2 3)")
  //   exec("(run '(request 'fetch \"https://httpbin.org/uuid\"))");
  // }, [])

const Home: React.FC = () => {
  const { data, refresh, ctx, tape, noteOrder, notes, loadNotes, newEmptyNote } = useStore()

  const noteViews = noteOrder.map((key: string) => {
    const note = notes[key]
    return <Note note={note} key={`${tape}-${note.key}`} />
  })

  const saveTape = async () => {
    const book = noteOrder.map((key: string) => {
      const { run, editorState } = notes[key]
      return {
        key,
        run,
        editorState: {
          doc: editorState.doc,
        },
      }
    })

    await Tape.save(Tape.make(ctx, book), "save")
  }

  const loadTape = async () => {
    const result = await Tape.load("save")
    if (result) {
      const { tape } = result
      console.log("playing tape", tape)
      Tape.play(ctx, tape)
      loadNotes(tape.book)
    }
  }

  return (
    <div className="absolute inset-0 flex flex-col bg-gray-100 dark:bg-black">
      <Titlebar saveTape={saveTape} loadTape={loadTape} />
      <div className="flex flex-col gap-2">
        {noteViews}
        <button
          onClick={() => newEmptyNote()}
          className="mx-auto text-gray-400 hover:text-gray-500 mt-1"
        >
          <IoAddCircleOutline size={30} title="Add new code block" />
        </button>
      </div>
    </div>
  )
}

const Note = ({ note }: { note: Note }) => {
  const { key, run } = note
  const { ctx, data, refresh, setNoteRun, setNoteEditorState } = useStore()

  const exec = (code: string, how: "run" | "debug") => {
    const src = ctx.read(`(progn\n${code}\n)`)
    const run = ctx.api.wisp_run_init(ctx.heap, src)

    if (how == "run")
      ctx.api.wisp_run_eval(ctx.heap, run, 100_000)

    refresh()
    setNoteRun(key, run)
  }

  const genkey = () => {
    const key = ctx.api.wisp_genkey(ctx.heap)
    if (key == ctx.sys.zap) throw new Error("genkey")
    const newdata = refresh()
    const row = getRow(newdata, "sym", key)
    const symstr = getUtf8String(newdata, row.str)
    return symstr
  }

  function evaluation() {
    if (run) {
      const { err, val, way } = getRow(data, "run", run) as unknown as Run
      if (val != data.sys.nah && err == data.sys.nil && way == data.sys.top) {
        return <Result val={val} />
      } else {
        return <Debugger run={run} />
      }
    }
  }

  function onChange(x: any) {
    setNoteEditorState(key, x)
  }

  return (
    <div className="flex gap-2 m-2 flex-col md:flex-row">
      <div className="w-full bg-white border-gray-300 dark:bg-neutral-900 dark:border-neutral-600 dark:text-neutral-400">
        <Editor exec={exec} genkey={genkey} initialState={note.editorState} onChange={onChange} />
      </div>
      {evaluation()}
    </div>
  )
}

const Titlebar = ({ saveTape, loadTape }) => {
  const classes = `
    flex justify-between items-center
    border-b
    pl-4 pr-2 py-1
    bg-slate-50 dark:bg-stone-800
    border-gray-300 dark:border-neutral-600 dark:border-b-neutral-700
  `

  const { user, isAuthenticated, isLoading, loginWithRedirect } = useAuth0()

  console.log({ user })

  return (
    <header className={classes}>
      <span className="tracking-tight text-gray-800 dark:text-neutral-400 flex items-center gap-4">
        <span className="flex items-center gap-1">
          <a href="https://github.com/mbrock/wisp"
             className="tracking-tight text-blue-900 dark:text-stone-500 flex column items-center"
             target="_blank">
            <VscGithub />
          </a>
        </span>
        <span className="font-semibold">Wisp.<span className="font-normal">Town</span></span>
      <span className="text-stone-500">v{wispPackageJSON.version}</span>
      </span>
      <div className="flex items-center">
        {
          isLoading
            ? null
            : (
              isAuthenticated
                ? (
                  <div className="flex gap-2 items-center dark:text-stone-400/80 text-xs font-medium">
                    {user.name}
                  </div>
                ) : (
                  <IconButton action={() => loginWithRedirect()} left right>
                    {"Log in"}
                  </IconButton>
                )
            )
        }
      </div>
    </header>
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

  let ctx = new Wisp(instance)

  ReactDOM.render(
    <Provider createStore={createStore(ctx)}>
      <Auth0Provider
        domain="dev-wnks73rd.us.auth0.com"
        clientId="tJwSob2zIUMr0Di0sHM46CsYcLz70r10"
        redirectUri={window.location.origin}>
        <Home />
      </Auth0Provider>
    </Provider>,
    document.querySelector("#app")
  )
}
