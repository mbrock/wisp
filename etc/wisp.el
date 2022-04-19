(define-derived-mode wisp-mode
  lisp-mode "Wisp"
  "Major mode for Wisp code."
  (setq-local inferior-lisp-program
              "zig build run -- repl")

  (define-common-lisp-style "wisp"
    "This style is for Wisp."
    (:inherit "basic")
    (:eval
     (whitespace-mode -1))
    (:variables
     (fill-column 64))
    (:indentation
     (fn (as lambda))
     (callback (as lambda))
     (handle (4 &rest (&whole 2 &rest 1)))
     (try (2 &rest (&whole 2 &rest 1)))
     (call-with-prompt 2)
     (call-with-parameter 2)
     (with 2)
     (returning 1)
     (for-each 1)
     (vector-for-each 1)
     (if 2)
     (when-result 2)
     (tag (4 2 &rest (&whole 2 &rest 1)))
     (response 2)
     (js-call 2)
     (serve 1)
     (run-command! 1)
     (authenticate! 1)
     (find-result 1)
     (defroute 2)
     (authenticate! 1)
     (find 1)
     (section 1)
     (article 0)
     (js-catch 1)
     (js-then 1)
     (js-call-function 1)
     (js-set! 2)
     (note 1)
     (do 0)
     (for-each-vector-index 1)
     (with 2)
     ))

  (common-lisp-set-style "wisp"))

(defun wisp-debug-region (start end)
  (interactive "r")
  (comint-send-string (inferior-lisp-proc) "(defvar *wisp-debug-run* (run (quote ")
  (comint-send-region (inferior-lisp-proc) start end)
  (comint-send-string (inferior-lisp-proc) ")))\n")
  (let ((buffer (generate-new-buffer "*wisp debug*")))
    (display-buffer buffer '(display-buffer-pop-up-window))
    (with-current-buffer buffer
      (wisp-mode 1)

      )))

(defun wisp-debug-last-sexp ()
  (interactive)
  (wisp-debug-region (save-excursion (backward-sexp) (point)) (point)))

(add-to-list 'auto-mode-alist '("\\.wisp\\'" . wisp-mode))

(provide 'wisp)
