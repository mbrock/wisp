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
                  exec(view.state.doc.sliceString(0))
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
          ],
        }),
      })

      view.focus()

      setEditorView(view)
    }
  }

  return (
    <div ref={install}
      className="border-t"
      />
  )
}
