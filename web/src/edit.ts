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

import { wisp } from "./lang"

import * as DOM from "incremental-dom"

import { drawSelection, dropCursor, EditorView, highlightActiveLine, highlightSpecialChars, keymap } from '@codemirror/view';
import { EditorState } from '@codemirror/state';
import { lineNumbers, highlightActiveLineGutter } from '@codemirror/gutter';
import { history, historyKeymap } from '@codemirror/history';
import { defaultKeymap, emacsStyleKeymap, indentWithTab } from '@codemirror/commands';
import { bracketMatching } from '@codemirror/matchbrackets';
import { defaultHighlightStyle } from '@codemirror/highlight';
import { HighlightStyle, tags } from '@codemirror/highlight';
import { rectangularSelection } from "@codemirror/rectangular-selection";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/closebrackets";
import { foldGutter, foldKeymap } from "@codemirror/fold";
import { indentOnInput } from "@codemirror/language";
import { autocompletion, completionKeymap } from "@codemirror/autocomplete";
import { commentKeymap } from "@codemirror/comment";
import { Panel, showPanel } from "@codemirror/panel"

export type RunMode = "run" | "debug"

export function startEditor(
  element: HTMLElement,
  exec: (code: string, how: RunMode) => void,
  doc: string,
) {
  function runCode(view: EditorView, how: "run" | "debug") {
    exec(view.state.doc.sliceString(0), how)
  }

  const view = new EditorView({
    parent: element,
    dispatch: tx => view.update([tx]),
    state: makeEditorState(doc),
  })

  view.focus()

  async function openFile(view: EditorView) {
    console.log("open file clicked")
    let [handle] = await window.showOpenFilePicker({
      types: [{
        description: "Wisp Source Files",
        accept: {
          "text/plain": [".wisp"],
        },
      }],
    })

    let file = await handle.getFile()
    let data = await file.text()

    view.setState(makeEditorState(data))
  }

  function makeEditorState(doc: string) {
    return EditorState.create({
      doc,
      extensions: [
        EditorView.theme({
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
            fontFamily: "dm mono, ui-monospace, SFMono-Regular, Menlo, monospace",
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
        lineNumbers(),
        highlightActiveLineGutter(),
        highlightSpecialChars(),
        history(),
        foldGutter(),
        drawSelection(),
        dropCursor(),
        EditorState.allowMultipleSelections.of(true),
        indentOnInput(),
        defaultHighlightStyle.fallback,
        HighlightStyle.define([{
          tag: tags.keyword,
          color: "yellow",
        }]).extension,
        bracketMatching(),
        closeBrackets(),
        autocompletion(),
        rectangularSelection(),
        highlightActiveLine(),
        EditorView.lineWrapping,
        keymap.of([
          {
            key: "Ctrl-Enter",
            run(view) {
              runCode(view, "run")
              return true
            }
          },
          {
            key: "Ctrl-Shift-Enter",
            run(view) {
              runCode(view, "debug")
              return true
            },
          },
          ...closeBracketsKeymap,
          ...defaultKeymap,
          ...emacsStyleKeymap,
          ...historyKeymap,
          indentWithTab,
          ...foldKeymap,
          ...commentKeymap,
          ...completionKeymap,
        ]),

        showPanel.of(view => {
          let div = document.createElement("div")

          DOM.patch(div, () => {
            DOM.elementOpen(
              "button", null, null,
              "class", "bg-stone-700 border border-stone-500 m-1 px-2 text-sm",
              "onclick", () => openFile(view))
            DOM.text("Open file")
            DOM.elementClose("button")

            DOM.elementOpen(
              "button", null, null,
              "class", "bg-stone-700 border border-stone-500 m-1 px-2 text-sm",
              "onclick", () => runCode(view, "run"))
            DOM.text("Evaluate buffer")
            DOM.elementClose("button")
          })

          return {
            dom: div,
            top: true,
          }
        }),

        wisp(),
      ],
    })
  }
}
