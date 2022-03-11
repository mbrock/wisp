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
import { defaultKeymap, indentWithTab } from '@codemirror/commands';
import { bracketMatching } from '@codemirror/matchbrackets';
import { defaultHighlightStyle } from '@codemirror/highlight';
import { rectangularSelection } from "@codemirror/rectangular-selection";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/closebrackets";
import { foldGutter, foldKeymap } from "@codemirror/fold";
import { indentOnInput } from "@codemirror/language";
import { autocompletion, completionKeymap } from "@codemirror/autocomplete";
import { commentKeymap } from "@codemirror/comment";
import { showPanel } from "@codemirror/panel";

export function Editor({ exec }: { exec: (code: string) => void }) {
  let [editorView, setEditorView] = React.useState<EditorView>(null)

  const install = (ref?: HTMLDivElement) => {
    if (ref && !editorView) {
      const view = new EditorView({
        parent: ref,
        state: EditorState.create({
          doc: "",
          extensions: [
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
            bracketMatching(),
            closeBrackets(),
            autocompletion(),
            rectangularSelection(),
            highlightActiveLine(),
            keymap.of([
              {
                key: "Ctrl-Enter",
                run(view) {
                  runCode(view)
                  return true
                }
              },
              ...closeBracketsKeymap,
              ...defaultKeymap,
              ...historyKeymap,
              indentWithTab,
              ...foldKeymap,
              ...commentKeymap,
              ...completionKeymap,
            ]),

            wisp(),

            showPanel.of(view => {
              const panel = (
                <div className="p-1 bg-yellow-50 border-t">
                  <button className={buttonClasses} onClick={() => runCode(view)}>
                    Run
                  </button>
                </div>
              )

              let dom = document.createElement("div")
              ReactDOM.render(panel, dom)

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

  const buttonClasses = `
    inline-flex items-center py-1 px-2 border border-gray-300 dark:border-neutral-600
    bg-white dark:bg-neutral-700
    text-gray-700 dark:text-neutral-300
    hover:bg-gray-50 dark:hover:bg-neutral-500
    font-medium
    focus:ring-1 focus:ring-indigo-500
    rounded-md
    text-xs
  `

  function runCode(view: EditorView) {
    exec(`(progn ${view.state.doc.sliceString(0)})`)
  }

  return (
    <div ref={install} />
  )
}
