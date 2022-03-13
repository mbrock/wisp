(define-derived-mode wisp-mode
  lisp-mode "Wisp"
  "Major mode for Wisp code."
  (setq-local inferior-lisp-program "zig build run"))

(provide 'wisp)
