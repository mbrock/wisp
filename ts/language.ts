import { parser } from "./wisp.grammar";
import { continuedIndent, delimitedIndent, foldInside, foldNodeProp, indentNodeProp, LanguageSupport, LRLanguage } from "@codemirror/language";
import { styleTags, tags } from "@codemirror/highlight";
import { completeFromList } from "@codemirror/autocomplete"

export const wispLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        Form: continuedIndent({ units: 2 }),
      }),
      foldNodeProp.add({
        Form: foldInside,
      }),
      styleTags({
        Symbol: tags.variableName,
        String: tags.string,
        Integer: tags.number,
        Comment: tags.lineComment,
        "( )": tags.paren,
      }),
    ],
  }),

  languageData: {
    commentTokens: { line: ";" },
    indentOnInput: /^\s*\)$/,
  },
})

export const wispCompletion = wispLanguage.data.of({
  autocomplete: completeFromList([
    {label: "defun", type: "keyword"},
    {label: "defvar", type: "keyword"},
    {label: "let", type: "keyword"},
    {label: "lambda", type: "keyword"},
    {label: "cons", type: "function"},
    {label: "car", type: "function"},
    {label: "cdr", type: "function"}
  ])
})

export function wisp() {
  return new LanguageSupport(wispLanguage, [wispCompletion])
}
