import { lezerPlugin } from "@nota-lang/esbuild-lezer"
import style from "esbuild-style-plugin"

import tailwindcss from "tailwindcss"
import autoprefixer from "autoprefixer"
import esbuild from "esbuild"

esbuild.build({
  entryPoints: ["src/wapp.ts"],
  outdir: "dist",
  bundle: true,
  sourcemap: true,
  entryNames: "[dir]/[name]-[hash]",
  loader: {
    ".wisp": "text",
    ".wasm": "file",
  },
  plugins: [
    lezerPlugin(),
  ],
})
