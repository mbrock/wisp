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

import { Wisp, View } from "./wisp"

import * as ReactDOM from "react-dom"
import * as React from "react"

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

    case "v08": {
      const str = data.str(val)
      return <div class="string">"{str}"</div>
    }

    case "duo": {
      const list = []
      let dotted = false
      let cur = val
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
              <Val data={data} val={x} key={i} />)
          }
        </div>
      )
    }

    case "fun": {
      const { env, par, exp } = data.row("fun", val)
      return (
        <table style={{ border: "1px solid #ccc", padding: "0.25rem" }}>
          <tbody>
            <tr>
              <td>env</td>
              <td><Val data={data} val={env} /></td>
            </tr>
            <tr>
              <td>par</td>
              <td><Val data={data} val={par} /></td>
            </tr>
            <tr>
              <td>exp</td>
              <td><Val data={data} val={exp} /></td>
            </tr>
          </tbody>
        </table>
      )
    }

    case "sym": {
      const { str, pkg, fun } = data.row("sym", val)
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
      return <span style={{ color: "red" }}>{val}</span>
  }
}

const Line = ({ data, turn, i }: {
  data: View,
  turn: Turn,
  i: number,
}) => {
  return (
    <div style={{ marginBottom: "0.5rem" }}>
      <span style={{ opacity: 0.4, padding: "0 1.5rem 0 0" }}>#{i}</span>
      <Val data={data} val={turn.exp} />
      <span style={{ padding: "0 1rem" }}>↦</span>
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

const WASI_ESUCCESS = 0
const WASI_EBADF = 8
const WASI_EINVAL = 28
const WASI_ENOSYS = 52
const WASI_STDOUT_FILENO = 1
const WASI_STDERR_FILENO = 2

class WASI {
  instance: WebAssembly.Instance

  setInstance(instance: WebAssembly.Instance) {
    this.instance = instance
  }

  getDataView(): DataView {
    return new DataView(this.instance.exports.memory.buffer)
  }

  exports() {
    return {
      proc_exit() {},

      fd_prestat_get() {},
      fd_prestat_dir_name() {},

      fd_write: (fd, iovs, iovsLen, nwritten) => {
        const view = this.getDataView()
        let written = 0
        let bufferBytes = []

        const buffers = Array.from({ length: iovsLen }, (_, i) => {
          const ptr = iovs + i * 8
          const buf = view.getUint32(ptr, !0)
          const bufLen = view.getUint32(ptr + 4, !0)

          return new Uint8Array(
            this.instance.exports.memory.buffer, buf, bufLen
          )
        })

        for (const iov of buffers) {
          for (var b = 0; b < iov.byteLength; b++)
            bufferBytes.push(iov[b])

          written += b
        }

        if (fd === WASI_STDOUT_FILENO)
          console.log(String.fromCharCode.apply(null, bufferBytes))
        else if (fd === WASI_STDERR_FILENO)
          console.warn(String.fromCharCode.apply(null, bufferBytes))

        view.setUint32(nwritten, written, !0)

        return WASI_ESUCCESS;
      },

      fd_close() {},
      fd_read() {},

      path_open() {},
      path_rename() {},
      path_remove_directory() {},
      path_unlink_file() {},

      fd_filestat_get() {},

      random_get() {},
    }
  }
}

onload = async () => {
  const wasi = new WASI
  const instance = await WebAssembly.instantiateStreaming(
    fetch(window.wispWasmUrl), {
    wasi_snapshot_preview1: wasi.exports()
  })

  wasi.setInstance(instance.instance)

  const ctx = new Wisp(instance.instance)

  ReactDOM.render(
    <Home ctx={ctx} />,
    document.querySelector("#app")
  )
}
