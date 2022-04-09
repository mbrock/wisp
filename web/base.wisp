(defvar *window* (js-global-this))
(defvar *wisp* (js-get *window* "wisp"))
(defvar *document* (js-get *window* "document"))
(defvar *console* (js-get *window* "console"))
(defvar <promise> (js-get *window* "Promise"))

(defun new (constructor &rest args)
  (apply #'js-new (cons constructor args)))

(defun query-selector (selector &optional root)
  (js-call (or root *document*) "querySelector" selector))

(defun query-selector-all (selector &optional root)
  (print `(query-selector-all ,selector ,root))
  (js-call (or root *document*) "querySelectorAll" selector))

(defun promise? (x)
  (if (and (eq? 'external (type-of x))
           (js-get x "then"))
      t
    nil))

(defun log (x)
  (js-call *console* "log" X))

(defun alert (x)
  (js-call *window* "alert" x))

(defun json (str)
  (js-call (js-get *window* "JSON") "parse" str))

(defun js-then (p f)
  (js-call p "then" (make-pinned-value f)))

(defun js-catch (p f)
  (js-call p "catch" (make-pinned-value f)))

(defun async (f)
  (call-with-prompt :async f
    (fn (v k)
      (js-catch
          (js-then v
            (fn (x)
              (async (fn () (call k x)))))
        (fn (e)
          (nonlocal-error! k e))))))

(defun await (x)
  (send! :async x))

(defmacro fetch (url &rest opts)
  `(await (js-call *window* "fetch" ,url (js-object ,@opts))))

(defun response-text (x)
  (await (js-call x "text")))

(defmacro :article (&rest body)
  `(do ,@body))

(defmacro :section (title &rest body)
  `(do ,@body))

(defun css (clauses)
  (reduce #'string-append
          (map (fn (clause)
                 (string-append
                  (symbol-name (head clause))
                  ": "
                  (let ((value (second clause)))
                    (if (symbol? value)
                        (symbol-name value)
                      (if (integer? value)
                          (string-append
                           (print-to-string value) "px")
                        value))) "; "))
               clauses)
          ""))

(:section (basic binding to incremental dom)
  (defun make-callback (symbol)
    (make-pinned-value
     (fn (&rest args)
       (apply (symbol-function symbol) args))))

  (defmacro tag (tag-symbol attrs &rest body)
    (let ((tag-name-var (fresh-symbol!)))
      `(let* ((,tag-name-var (symbol-name ,tag-symbol)))
         (idom-open-start! ,tag-name-var)
         (for-each ,attrs
           (fn (attr)
             (idom-attr! (symbol-name (head attr))
                         (second attr))))
         (idom-open-end!)
         ,@body
         (idom-close! ,tag-name-var))))

  (defun text (text)
    (idom-text! text)))

(:section (rendering expressions to html)
  (defun render-sexp (sexp)
    (cond
      ((nil? sexp)
       (tag :div '((:class "wisp value list")) nil))
      ((symbol? sexp)
       (tag :span `((:class "wisp value symbol")
                    (:data-package-name
                     ,(package-name
                       (symbol-package sexp)))
                    (:data-symbol-name
                     ,(symbol-name sexp))
                    (:data-function-kind
                     ,(if (symbol-function sexp)
                          (if (jet? (symbol-function sexp))
                              "jet" "fun")
                        "")))
         (cond
           ((eq? sexp 'todo)
            (tag :input '((:type "checkbox")) nil))
           ((eq? sexp 'done)
            (tag :input '((:type "checkbox")
                          (:checked "checked")) nil))
           (t
            (do
              (tag :span '((:class "package-name"))
                (text (package-name (symbol-package sexp))))
              (tag :span '((:class "symbol-name"))
                (text (symbol-name sexp))))))))
      ((pair? sexp)
       (let* ((callee (head sexp))
              (tag-type (cond
                          ((eq? callee :section) :section)
                          ((eq? callee :article) :article)
                          (t :div))))
         (tag tag-type
           `((:class "wisp value list")
             (:data-callee
              ,(if (symbol? callee)
                   (string-append
                    (package-name (symbol-package callee))
                    ":"
                    (symbol-name callee))
                 "")))
           (render-list-contents sexp))))
      ((string? sexp)
       (tag :span '((:class "wisp value string"))
         (text sexp)))
      ((integer? sexp)
       (tag :span '((:class "wisp value number"))
         (text (print-to-string sexp))))
      ((eq? 'vector (type-of sexp))
       (tag :div '((:class "wisp value vector"))
         (render-vector-contents sexp 0)))
      ((eq? 'function (type-of sexp))
       (tag :i ()
         (if (function-name sexp)
             (text (symbol-name (function-name sexp)))
           (text "#<FUNCTION>")) ))
      ((eq? 'external (type-of sexp))
       (tag :i ()
         (text "EXTERN")))))

  (defun render-list-contents (sexp)
    (unless (nil? sexp)
      (render-sexp (head sexp))
      (let ((tail (tail sexp)))
        (if (list? tail)
            (render-list-contents tail)
          (do
            (tag :span '((:class "dot"))
              (text "·"))
            (render-sexp tail))))))

  (defun render-vector-contents (vector i)
    (vector-each vector #'render-sexp))

  (defun style (clauses)
    `(:style ,(css clauses))))


;;;;;

(defmacro defroute (pattern req-var &rest body)
  `(install-route ',pattern (fn (,req-var ,@(filter pattern #'symbol?))
                              ,(prognify body))))

(defmacro with-authentication (clause &rest body)
  `(authenticate! ,(head clause) (fn (,(second clause)) ,@body)))

(defun run-command! (cwd &rest cmd)
  (print `(run-command! ,cwd ,cmd))
  (let* ((process (js-call <deno> "run"
                    (js-object "cwd" cwd "cmd"
                               (vector-from-list cmd))))
         (status (await (js-call process "status"))))
    (if (eq? 0 (js-get status "code"))
        t
      (error `(command-error (cwd ,cwd) (cmd ,cmd))))))

(defun mkdir-recursive! (path)
  (await-call <deno> "mkdir" path (js-object "recursive" t)))

(defun response (status headers body)
  (new <response> body
       (js-object "status" status
                  "headers" (apply #'js-object headers))))

