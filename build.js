require("esbuild").build({
  entryPoints: ["ts/index.tsx"],
  outdir: "dist",
  bundle: true,
  sourcemap: true,
  entryNames: "[dir]/[name]-[hash]",
  loader: { ".wasm": "file" },
  plugins: [
    require("esbuild-style-plugin")({
      postcss: [
        require("tailwindcss"),
        require("autoprefixer"),
      ],
    })
  ],
})
