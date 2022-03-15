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
    inline-flex items-center py-1 px-2 border border-gray-300 dark:border-neutral-600
    bg-white dark:bg-neutral-700
    text-gray-700 dark:text-neutral-300
    hover:bg-gray-50 dark:hover:bg-neutral-500
    font-medium
    focus:ring-1 focus:ring-indigo-500
    ${left ? "rounded-l-md" : (right ? "rounded-r-md" : "")}
    ${disabled ? "text-gray-400 dark:text-neutral-500" : ""}
  `

  return (
    <button className={classes}
      disabled={disabled}
      onClick={action}>
      {children}
    </button>
  )
}
