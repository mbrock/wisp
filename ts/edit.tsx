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
import { IconButton } from "./button";
import { VscDebug, VscDebugContinue } from "react-icons/vsc";

export const Editor: React.FC<{
  exec: (code: string, how: "run" | "debug") => void,
  initialCode: string | undefined,
}> = ({
  exec, initialCode,
}) => {
  let [editorView, setEditorView] = React.useState<EditorView>(null)

  const install = (ref?: HTMLDivElement) => {
    if (ref && !editorView) {
      const view = new EditorView({
        parent: ref,
        state: EditorState.create({
          doc: initialCode,
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

              ".cm-scroller, .cm-tooltip.cm-tooltip-autocomplete > ul": {
                fontFamily: "DM Mono",
              }
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
            bracketMatching(),
            closeBrackets(),
            autocompletion(),
            rectangularSelection(),
            highlightActiveLine(),
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
              ...historyKeymap,
              indentWithTab,
              ...foldKeymap,
              ...commentKeymap,
              ...completionKeymap,
            ]),

            wisp(),

            showPanel.of(() => {
              let dom = document.createElement("div")
              ReactDOM.render(
                <div className="p-1 bg-gray-50 flex">
                  <IconButton action={() => runCode(view, "run")} left>
                    <VscDebugContinue title="Run code" />
                  </IconButton>
                  <IconButton action={() => runCode(view, "debug")} right>
                    <VscDebug title="Debug code" />
                  </IconButton>
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

  return (
    <div className="border h-full border-gray-300" ref={install} />
  )
}
