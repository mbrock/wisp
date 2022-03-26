const defaultTheme = require("tailwindcss/defaultTheme")

module.exports = {
  content: ["./src/*.tsx", "./index.css", "../core/lisp/web.wisp"],
  theme: {
    extend: {
      fontFamily: {
        sans: ["inter var", "inter", ...defaultTheme.fontFamily.sans],
        mono: ["dm mono", ...defaultTheme.fontFamily.mono],
      },
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
  ],
}
