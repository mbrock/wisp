(auth0-get-token)

(vector-for-each (readdir "/wisp6")
  (fn (x)
    (let ((stat (stat (string-append "/wisp6/" x))))
      (display
       (list (read-from-string (js-get stat "type"))
             x
             (js-get stat "size"))))))

(note (slide 1)
  (todo greetings to zig hackers)
  (todo wisp is a lisp for webassembly)
  (todo mostly for fun)
  (todo live at "https://wisp.town")
  (todo code at "https://github.com/mbrock/wisp"))

(map (fn (x) (* x x)) (list 1 2 3))

(try (* 2 (/ 1 0))
  (catch (e k)
    (vector-from-list (read-many-from-string (listen)))))

(listen)

(defun alert (x)
  (js-call *window* "alert" x))

(defun prompt (x)
  (js-call *window* "prompt" x))

(defun sleep (secs)
  (await
   (new <promise>
        (callback (resolve)
          (js-call *window* "setTimeout" resolve (* 1000 secs))))))

(do (sleep 2) "ok done")

(try
  (* 2 (/ 1 0))
  (catch (error continuation)
    (alert "oh no")
    (sleep 1)
    (call continuation
          (read-from-string
           (prompt
            (print-to-string error))))))

(note (todo slide 2)
  (novel emacs with html and css)
  (structure editing)
  (browser and server))

(note (todo slide 3)
  (delimited continuation control)
  (interactive restarts)
  (serializable heap))

(note (todo slide 4)
  (defroute ("POST" "git") req
    (with-authentication (req user-key)
      (let* ((repo-key (symbol-name (genkey!)))
             (repo-path (string-append "git/" repo-key)))
        (mkdir-recursive! repo-path)
        (run-command! repo-path "git" "init" "--bare")
        (run-command! repo-path "git" "config" "wisp.auth.push" user-key)
        (response 200 ()
          (string-append "https://git.wisp.town/" repo-key))))))

(note (todo slide 5)
  (set-keymap!
   (("f" "ArrowRight") forward-sexp!)
   (("b" "ArrowLeft")  backward-sexp!)
   (("C-f" "C-ArrowRight") forward-into-sexp!)
   (("C-b" "C-ArrowLeft") backward-into-sexp!)
   (("S-F" "S-ArrowRight") select-forward-sexp!)
   (("S-B" "S-ArrowLeft") select-backward-sexp!)
   (("u") up-sexp!)
   (("p" "ArrowUp") backward-line!)
   (("n" "ArrowDown") forward-line!)
   ("t" transpose!)
   ("k" delete!)
   ("d" duplicate!)
   (("C-g" "Escape") unselect!)
   ("i" start-editor!)
   ("e" evaluate-sexp!)
   ("s" save!)
   ("." goto-place-anywhere!)
   ("C-." goto-place-inside!)
   ("Tab" other-window!)))

(NOTE (:APRIL 1 2022) (DONE JS FFI) (TODO KEY :C TO CHANGE) (TODO PREVENT CURSOR SHENANIGANS) (DONE KEY (CTRL :E) TO EVAL TOP-LEVEL FORM) (TODO AUTOCOMPLETE ALL SYMBOLS) (TODO HANDLE EVALUATION ERRORS) (TODO REFACTOR WASD INTERFACE) (TODO CSS AS SEXP) (DONE READ MANY INSERTED FORMS) (DONE WHOLE DEXP NAVIGATION BY DEFAULT) (DONE LOCALSTORAGE SAVING))
