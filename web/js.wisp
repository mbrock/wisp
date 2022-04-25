(defvar *window* (js-global-this))
(defvar *wisp* (js-get *window* "wisp"))
(defvar *document* (js-get *window* "document"))
(defvar *console* (js-get *window* "console"))
(defvar <promise> (js-get *window* "Promise"))

(defmacro callback (args &rest body)
  `(make-pinned-value
    (fn ,args ,@body)))

(defun make-callback (symbol)
  (callback (&rest args)
    (apply (symbol-function symbol) args)))

(defun js-get* (object indices)
  (if (nil? indices)
      object
    (js-get* (js-get object (head indices))
             (tail indices))))

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
  (if (promise? p)
      (js-call p "then" (make-pinned-value f))
    (call f p)))

(defun js-catch (p f)
  (if (promise? p)
      (js-call p "catch" (make-pinned-value f))
    p))

(defun async (f)
  (call-with-prompt :async f
    (fn (v k)
      (js-catch
          (js-then v
            (fn (x)
              (async (fn () (call k x)))))
        (fn (e)
          (log e)
          (nonlocal-error! k e))))))

(defun await (x)
  (send! :async x))

(defmacro fetch (url &rest opts)
  `(await (js-call *window* "fetch" ,url (js-object ,@opts))))

(defun response-text (x)
  (await (js-call x "text")))

(defun sleep (secs)
  (await
   (new <promise>
        (callback (resolve)
          (js-call *window* "setTimeout" resolve (* 1000 secs))))))
