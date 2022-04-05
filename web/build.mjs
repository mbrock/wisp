import { lezerPlugin } from "@nota-lang/esbuild-lezer"

import esbuild from "esbuild"

esbuild.build({
  entryPoints: [
    "lib/codemirror.ts",
    "lib/git.ts",
    "lib/idom.ts",
    "lib/wisplang.grammar",
  ],
  outdir: "lib",
  bundle: true,
  sourcemap: true,
  format: "esm",
  loader: {
    ".wisp": "text",
    ".wasm": "file",
  },
  plugins: [
    lezerPlugin(),
  ],
})
