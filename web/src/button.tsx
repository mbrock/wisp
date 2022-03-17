import * as React from "react"

export const IconButton: React.FC<{
  action: () => void,
  left?: boolean,
  right?: boolean,
  disabled?: boolean,
}> = ({
  action, left, right, disabled, children,
}) => {
  const classes = `
    inline-flex items-center
    py-1 px-2
    text-xs font-semibold

    bg-white dark:bg-stone-700/50
    text-gray-700 dark:text-stone-400
    border border-gray-300 dark:border-neutral-600
    hover:bg-gray-50 dark:hover:bg-neutral-500

    ${disabled ? " text-gray-400 dark:text-neutral-500" : ""}
    ${left ? " rounded-l-md" : ""}
    ${right ? " rounded-r-md" : ""}
  `

  return (
    <button className={classes}
      disabled={disabled}
      onClick={action}>
      {children}
    </button>
  )
}
