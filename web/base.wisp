(defvar *window* (js-global-this))
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
