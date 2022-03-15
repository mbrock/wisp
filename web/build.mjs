import { lezerPlugin } from "@nota-lang/esbuild-lezer"
import style from "esbuild-style-plugin"

import tailwindcss from "tailwindcss"
import autoprefixer from "autoprefixer"
import esbuild from "esbuild"

esbuild.build({
  entryPoints: ["src/index.tsx"],
  outdir: "dist",
  bundle: true,
  sourcemap: true,
  entryNames: "[dir]/[name]-[hash]",
  loader: {
    ".wasm": "file",
    ".ttf": "file",
    ".woff2": "file",
    ".woff": "file",
  },
  plugins: [
    style({
      postcss: [
        tailwindcss,
        autoprefixer,
      ],
    }),
    lezerPlugin(),
  ],
})
