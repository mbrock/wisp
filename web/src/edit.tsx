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

import { wisp } from "./language"

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
import { showPanel } from "@codemirror/panel";
import { IconButton } from "./button";
import { VscDebug, VscDebugContinue, VscRuby } from "react-icons/vsc";

export const Editor: React.FC<{
  exec: (code: string, how: "run" | "debug") => void,
  genkey: () => string,
  initialState: any | null,
  onChange: (state: any) => void,
}> = ({
  exec, genkey, initialState, onChange,
}) => {
  let [editorView, setEditorView] = React.useState<EditorView>(null)

  const install = (ref?: HTMLDivElement) => {
    if (ref && !editorView) {
      console.log("new editor view", { initialState })
      const view = new EditorView({
        parent: ref,
        dispatch: tx => {
          view.update([tx])
          onChange(view.state.toJSON())
        },
        state: EditorState.create({
          ...(initialState || {}),
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
              {
                key: "Ctrl-Shift-k",
                run(view) {
                  insertGenkey(view)
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

            wisp(),

                  // <div>
                  //   <IconButton action={() => runCode(view, "run")} left>
                  //     <VscDebugContinue title="Run code" />
                  //   </IconButton>
                  //   <IconButton action={() => runCode(view, "debug")} right>
                  //     <VscDebug title="Debug code" />
                  //   </IconButton>
                  // </div>
                  // <div className="hidden">
                  //   <IconButton action={() => insertGenkey(view)} right>
                  //     <VscRuby title="Insert new key" />
                  //   </IconButton>
                  // </div>

            showPanel.of(() => {
              let dom = document.createElement("div")
              ReactDOM.render(
                <div className="p-1 leading-tight bg-stone-900 text-amber-100/90 flex justify-between font-mono px-2">
                  <div className="font-medium pr-1">*scratch*</div>
                  <div className="text-amber-100/70">Wisp</div>
                  <div>{"Ctrl-Enter to run"} </div>
                </div>,
                dom)

              return {
                dom,
                top: true,
              }
            }),
          ],
        }),
      })

      view.focus()

      setEditorView(view)
    }
  }

  function runCode(view: EditorView, how: "run" | "debug") {
    exec(view.state.doc.sliceString(0), how)
  }

  function insertGenkey(view: EditorView) {
    const key = genkey()
    view.dispatch(
      view.state.update(
        view.state.changeByRange(range => {
          return {
            range: range.extend(range.from, range.from + key.length),
            changes: {
              from: range.from,
              insert: key,
            },
          }
        })
      )
    )
  }

  return (
    <div className="border border-neutral-900" ref={install} />
  )
}
