const defaultTheme = require("tailwindcss/defaultTheme")

module.exports = {
  content: ["./ts/*.tsx", "./index.css"],
  theme: {
    extend: {
      fontFamily: {
        sans: ["inter var", ...defaultTheme.fontFamily.sans],
      },
    },
  },
  plugins: [
    require("@tailwindcss/forms"),
  ],
}
