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

import idom from "./lib/idom.js"
import git from "./lib/git.js"
import cm from "./lib/codemirror.js"
import * as grammar from "./lib/wisplang.js"
import { Wisp, WASD, domCode } from "./wisp.js"
import WASI from "./wasi.js"

onload = async () => {
  const wasi = new WASI
  const wasd = new WASD

  const { instance } = await WebAssembly.instantiateStreaming(
    fetch("dist/wisp.wasm"), {
      wasi_snapshot_preview1: wasi.exports(),
      dom: wasd.exports(),
    }
  )

  const exports = instance.exports

  wasi.setMemory(exports.memory)

  let ctx = new Wisp(instance)

  wasd.setWisp(ctx)

  function exec(code) {
    const src = ctx.read(`
      (with-simple-error-handler ()
        ${code}
      )`)
    const run = ctx.api.wisp_run_init(ctx.heap, src)
    ctx.api.wisp_run_eval(ctx.heap, run, 4_000_000)
  }

  const basecode = await fetch("./base.wisp").then(x => x.text())
  const dexpcode = await fetch("./dexp.wisp").then(x => x.text())
  const usercode = await fetch("./user.wisp").then(x => x.text())
  const democode = await fetch("./demo.wisp").then(x => x.text())
  const servcode = await fetch("./server.wisp").then(x => x.text())
  
  exec(basecode)
  exec(dexpcode)
  exec(usercode)

  const file = localStorage.getItem("wisp-file") || democode
  const forms = file ? ctx.readMany(file) : ctx.sys.nil
  let packageName = "WISP"
  let functionName = "WISP-BOOT"
  let pkgname = ctx.allocString(packageName)
  let funname = ctx.allocString(functionName)
  let result = ctx.api.wisp_call_package_function(
    ctx.heap,
    pkgname, packageName.length,
    funname, functionName.length,
    forms,
  )

  if (result === ctx.sys.zap)
    throw new Error

  ctx.free_0(pkgname, funname)
}

function startEditor(
  element, doc, symbols, done,
) {
  function onSubmit(view) {
    done(view.state.doc.sliceString(0))
  }

  const view = new cm.view.EditorView({
    parent: element,
    dispatch: tx => view.update([tx]),
    state: makeEditorState(doc),
  })

  view.focus()

  function makeEditorState(doc) {
    return cm.state.EditorState.create({
      doc,
      extensions: [
        cm.view.EditorView.theme({
          ".cm-completionIcon": {
            width: "1.5em",
          },

          ".cm-focused": {
            outline: "none !important",
          },

          "&": {
            fontSize: 16,
          },

          ".cm-scroller, .cm-content, .cm-tooltip.cm-tooltip-autocomplete > ul": {
            fontFamily: "berkeley mono, dm mono, ui-monospace, SFMono-Regular, Menlo, monospace",
            fontWeight: "normal",
          },

          ".cm-gutters": {
              backgroundColor: "#2223 !important",
          },

          ".cm-activeLineGutter": {
              backgroundColor: "#2228 !important"
          },

          ".cm-activeLine": {
              backgroundColor: "#1119 !important"
          },

          ".cm-panels": {
            backgroundColor: "#222 !important",
            zIndex: 2,
            borderBottom: "1px solid #555",
            padding: 5,
          },
        }, {
          dark: true,
        }),
        
        cm.highlight.defaultHighlightStyle.fallback,
        cm.highlight.HighlightStyle.define([{
          tag: cm.highlight.tags.keyword,
          color: "yellow",
        }]).extension,
        
        cm.view.highlightSpecialChars(),
        cm.view.drawSelection(),
        cm.view.dropCursor(),
        cm.view.highlightActiveLine(),
        
        cm.language.indentOnInput(),
        
        cm.matchbrackets.bracketMatching(),
        cm.closebrackets.closeBrackets(),
        cm.autocomplete.autocompletion(),
        
        cm.view.EditorView.lineWrapping,
        
        cm.view.keymap.of([
          {
            key: "Enter",
            run(view) {
              onSubmit(view)
              return true
            }
          },
          cm.commands.indentWithTab,
          ...cm.closebrackets.closeBracketsKeymap,
          ...cm.commands.defaultKeymap,
          ...cm.commands.emacsStyleKeymap,
          ...cm.comment.commentKeymap,
          ...cm.autocomplete.completionKeymap,
        ]),

        wisplang(symbols),
      ],
    })
  }
}

const hang = n => node => {
  let kids = node.getChildren("Form")
  let first = kids[n - 1], last = node.lastChild
  if (first && first.to < last.from) {
    return {
      from: first.to,
      to: last.type.isError ? node.to : last.from
    }
  }
  return null
}

let wispLanguage = cm.language.LRLanguage.define({
  parser: grammar.parser.configure({
    props: [
      cm.language.indentNodeProp.add({
        Defun: cm.language.continuedIndent(),
        Let: cm.language.continuedIndent(),
        Fn: cm.language.continuedIndent(),
      }),
      cm.language.foldNodeProp.add({
        Defun: hang(2),
        Let: hang(1),
        Fn: hang(1),
      }),
      cm.highlight.styleTags({
        "defun let fn run": cm.highlight.tags.keyword,
        Symbol: cm.highlight.tags.variableName,
        Key: cm.highlight.tags.docComment,
        ZB32: cm.highlight.tags.meta,
        Date: cm.highlight.tags.number,
        String: cm.highlight.tags.string,
        Integer: cm.highlight.tags.integer,
        Comment: cm.highlight.tags.lineComment,
        "( )": cm.highlight.tags.paren,
      }),
    ],
  }),

  languageData: {
    commentTokens: { line: ";" },
    indentOnInput: /^\s*\)$/,
    closeBrackets: {
      brackets: ["("],
      before: `");`,
    },
  },
})

let wispCompletion = symbols =>
  wispLanguage.data.of({
    autocomplete: cm.autocomplete.completeFromList(
      symbols.map(x => ({
        label: x,
        type: "function",
      }))
    )
  })

function wisplang(symbols) {
  return new cm.language.LanguageSupport(
    wispLanguage,
    [wispCompletion(symbols)]
  )
}


window.fs = new git.fs("wisp", { wipe: true })
window.git = git.git
window.git_http = git.http

window.wisp = {
  startEditor,
  domCode,
}

