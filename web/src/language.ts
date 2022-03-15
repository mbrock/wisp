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

import { parser } from "./wisp.grammar"
import { styleTags, tags } from "@codemirror/highlight"
import { completeFromList } from "@codemirror/autocomplete"
import {
  continuedIndent, foldNodeProp, indentNodeProp,
  LanguageSupport, LRLanguage
} from "@codemirror/language"
import { SyntaxNode } from "@lezer/common"

const hang = (n: number) => (node: SyntaxNode) => {
  let kids = node.getChildren("Form")
  let first = kids[n - 1], last = node.lastChild;
  if (first && first.to < last.from) {
    return {
      from: first.to,
      to: last.type.isError ? node.to : last.from
    }
  }
  return null
}

export const wispLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        Defun: continuedIndent(),
        Let: continuedIndent(),
        Lambda: continuedIndent(),
      }),
      foldNodeProp.add({
        Defun: hang(2),
        Let: hang(1),
        Lambda: hang(1),
      }),
      styleTags({
        "defun let lambda run": tags.keyword,
        Symbol: tags.variableName,
        Key: tags.docComment,
        ZB32: tags.meta,
        Date: tags.number,
        String: tags.string,
        Integer: tags.integer,
        Comment: tags.lineComment,
        "( )": tags.paren,
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

export const wispCompletion = wispLanguage.data.of({
  autocomplete: completeFromList([
    {label: "run", type: "keyword"},

    {label: "defun", type: "keyword"},
    {label: "defvar", type: "keyword"},
    {label: "let", type: "keyword"},
    {label: "call/cc", type: "keyword"},
    {label: "lambda", type: "keyword"},

    {label: "list", type: "function"},
    {label: "append", type: "function"},
    {label: "cons", type: "function"},
    {label: "car", type: "function"},
    {label: "cdr", type: "function"},
  ])
})

export function wisp() {
  return new LanguageSupport(wispLanguage, [wispCompletion])
}
