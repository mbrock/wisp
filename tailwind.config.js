const defaultTheme = require("tailwindcss/defaultTheme")

module.exports = {
  content: ["./ts/*.tsx", "./index.css"],
  theme: {
    extend: {
      fontFamily: {
        sans: ["inter var", ...defaultTheme.fontFamily.sans],
        mono: ["dm mono", ...defaultTheme.fontFamily.mono],
      },
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
  ],
}
