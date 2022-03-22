(define-derived-mode wisp-mode
  lisp-mode "Wisp"
  "Major mode for Wisp code."
  (setq-local inferior-lisp-program "zig build run -- repl")

  (define-common-lisp-style "wisp"
    "This style is for Wisp."
    (:inherit "basic")
    (:eval
     (whitespace-mode 1))
    (:variables
     (fill-column 64))
    (:indentation
     (fn (as lambda))
     (handle (4 &rest (&whole 2 &rest 1)))
     (try (2 &rest (&whole 2 &rest 1)))
     (call-with-prompt 1)
     (returning 1)))

  (common-lisp-set-style "wisp"))

(provide 'wisp)
