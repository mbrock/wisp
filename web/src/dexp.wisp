;; -*- mode: wisp; fill-column: 64; -*-
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; * Basic binding to Incremental DOM

(defun make-callback (symbol)
  (make-pinned-value
   (fn (&rest args)
     (apply (symbol-function symbol) args))))

(defmacro tag (tag-symbol attrs &rest body)
  (let ((tag-name (symbol-name tag-symbol)))
    `(progn
       (idom-open-start! ,tag-name)
       (for-each ,attrs
         (fn (attr)
           (idom-attr! (symbol-name (head attr))
                      (second attr))))
       (idom-open-end!)
       ,@body
       (idom-close! ,tag-name))))

(defun text (text)
  (idom-text! text))


;;; * D-expressions

(defun render-sexp (sexp)
  (cond
    ((nil? sexp)
     (tag :div '((:class "list")) nil))
    ((symbol? sexp)
     (tag :span `((:class "symbol")
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
          (progn
            (tag :span '((:class "package-name"))
              (text (package-name (symbol-package sexp))))
            (tag :span '((:class "symbol-name"))
              (text (symbol-name sexp))))))))
    ((pair? sexp)
     (tag :div `((:class "list")
                 (:data-callee ,(if (symbol? (head sexp))
                                    (string-append
                                     (package-name (symbol-package (head sexp)))
                                     ":"
                                     (symbol-name (head sexp)))
                                  "")))
       (render-list-contents sexp)))
    ((string? sexp)
     (tag :span '((:class "string"))
       (text sexp)))
    ((integer? sexp)
     (tag :span '((:class "number"))
       (text (print-to-string sexp))))
    ((eq? 'vector (type-of sexp))
     (tag :div '((:class "vector"))
       (render-vector-contents sexp 0)))
    ((eq? 'function (type-of sexp))
     (tag :i ()
          (if (function-name sexp)
              (text (symbol-name (function-name sexp)))
            (text "#<anonymous-function>")) ))
    ((eq? 'external (type-of sexp))
     (tag :i ()
          (text "extern")))))

(defun render-list-contents (sexp)
  (unless (nil? sexp)
    (render-sexp (head sexp))
    (let ((tail (tail sexp)))
      (if (list? tail)
          (render-list-contents tail)
        (progn
          (tag :span '((:class "dot"))
            (text "·"))
          (render-sexp tail))))))

(defun vector-each (vector function)
  (vector-each-loop vector function 0))

(defun vector-each-loop (vector function i)
  (when (< i (vector-length vector))
    (call function (vector-get vector i))
    (vector-each-loop vector function (+ i 1))))

(defun render-vector-contents (vector i)
  (vector-each vector #'render-sexp))

(defun draw-app (forms)
  (tag :style () (text "
    body { font: 16px 'berkeley mono', 'dm mono', 'inconsolata', monospace; }
    #wisp-app { margin: 10px; }

    .symbol { text-transform: lowercase; }

    .string:before { content: '“'; }
    .string:after { content: '”'; }

    .string { color: lightgray; }

    .list > [data-function-kind=jet]:first-of-type { color: goldenrod; }
    .list > [data-function-kind=fun]:first-of-type { color: lightsalmon; }

    .vector, .list {
      display: flex;
      flex-wrap: wrap;
      gap: 5px;
      align-items: center;
      margin: 0 5px;
      padding: 0 5px;
      max-width: 60ch;
      min-height: 1em;
    }

    .list[data-callee='WISP:COND'] > :not(.cursor) {
      width: 100%;
    }

    .list[data-callee='WISP:DEFUN'] > div:first-of-type ~ :not(.cursor),
    .list[data-callee='WISP:NOTE'] > div:first-of-type ~ :not(.cursor)
    {
      width: 100%;
    }

    .symbol[data-package-name=WISP] > .package-name { display: none; }
    .symbol[data-package-name=KEYWORD] > .package-name { display: none; }

    .package-name:after,
    .symbol[data-package-name=KEYWORD] > .symbol-name:before {
      content: \":\";
      opacity: 0.7;
      padding-right: 1px;
    }

    .list { border: 0 solid #555; border-width: 0 2px; border-radius: 10px; }
    .vector {
      border: 0 solid #556a;
      border-width: 1px 3px;
      border-radius: 10px;
      padding: 5px;
    }

    @keyframes blink {
      0%, 100% { background: #ffa8 } 50% { background: #ffaa }
    }

    .cursor:empty:before {
      content: ' ';
      height: 6px; width: 6px; border-radius: 100%;
      animation: blink 1s infinite;
    }

    .cursor {
      display: inline-flex; flex-wrap: wrap; align-items: center; gap: 5px;
    }

    .cursor:not(:empty) {
      background: #63ffeb40; border-radius: 5px;
      margin: 0 5px; padding: 0 5px;
    }

    ins { text-decoration: none; }

    article {
      display: flex;
      justify-content: space-between;
    }

    header { display: flex; flex-direction: column; align-items: end; }
  "))
  (tag :article ()
    (tag :main ()
      (tag :div
        '((:style "display: inline-flex; flex-direction: column; gap: 20px")
          (:id "file"))
        (tag :ins '((:class "cursor")) nil)
        (for-each forms #'render-sexp)))

    (tag :header '((:id "eval-output")) nil))

  (progn
    (idom-patch!
     (query-selector "#eval-output")
     *render-sexp-callback* *eval-output*)
    (set! *cursor* (query-selector ".cursor"))
    (print (list :cursor *cursor*))))

(defvar *initial-forms*
  '((note (metadata)
     (spdx-license-identifier "AGPL-3.0-or-later")
     (source-code-url "https://github.com/mbrock/wisp"))
    (defun foo (x) (+ x 2 (* 3 4)))
    (foo 10)
    (note (wisp keymap)
     [((f) forward-over)
     ((b) backward-over)
     ((ctrl f) forward-into)
     ((ctrl b) backward-into)
     ((n) forward-line)
     ((p) backward-line)
     ((shift f) select-forward)
     ((escape) unselect)
     ((t) transpose!)
     ((k) delete!)
     ((d) duplicate!)
     ((i) insert!)
     ((e) evaluate!)])
    (note (:march 31 2022)
     (done implement inserting in structural editor)
     (done structure to code string)
     (done evaluating expressions)
     (todo saving file))
    (note (:march 29 2002)
     (done implement structural editor)
     (done play around)
     (todo buy bananas))))

(defun query-selector (selector)
  (js-call *document* "querySelector" selector))

(with-simple-error-handler ()
  (defvar *window* (js-global-this))
  (defvar *document* (js-get *window* "document"))
  (defvar *wisp* (js-get *window* "wisp"))
  (defvar *eval-output* '())
  (defvar *root-element* (query-selector "#wisp-app"))
  (defvar *key-callback* (make-callback 'on-keydown))
  (defvar *insert-callback* (make-callback 'on-insert))
  (defvar *render-callback* (make-callback 'draw-app))
  (defvar *render-sexp-callback* (make-callback 'do-render-sexp))
  (defvar *cursor* nil))

(defun render-app (forms)
  (with-simple-error-handler ()
    (idom-patch! *root-element* *render-callback* forms)))

(defun %vector-element-bindings (vector names)
  (let ((i 0))
    (map (fn (name)
           (returning `(,name (vector-get ,vector ,i))
             (set! i (+ i 1)))) names)))

(defmacro with-vector-elements (vector names &rest body)
  (let (($vector (fresh-symbol!)))
    `(let ((,$vector ,vector))
       (let ,(%vector-element-bindings $vector names)
         ,(prognify body)))))

(defun on-keydown (key-info)
  (with-simple-error-handler ()
    (with-vector-elements key-info (key control? shift? alt? meta? repeat?)
      (print (list 'keydown key control? shift? alt? meta? repeat?))
      (cond
        ((or (string-equal? key "ArrowRight")
             (string-equal? key "f"))
         (forward! :forward control?))
        ((or (string-equal? key "ArrowLeft")
             (string-equal? key "b"))
         (forward! :backward control?))
        ((or (string-equal? key "F")
             (and shift? (string-equal? key "ArrowRight")))
         (select! :forward))
        ((or (string-equal? key "B")
             (and shift? (string-equal? key "ArrowLeft")))
         (select! :backward))
        ((or (string-equal? key "ArrowUp")
            (string-equal? key "p"))
         (goto-next-line! :backward))
        ((or (string-equal? key "ArrowDown")
             (string-equal? key "n"))
         (goto-next-line! :forward))
        ((string-equal? key "t")
         (transpose!))
        ((string-equal? key "k")
         (delete!))
        ((string-equal? key "d")
         (duplicate!))
        ((string-equal? key "Escape")
         (unselect!))
        ((string-equal? key "i")
         (start-editor!))
        ((string-equal? key "e")
         (eval! control?))
        ((string-equal? key "(")
         (progn
           (insert-code! "()")
           (forward! :backward nil)))
        ((string-equal? key "s")
         (save!))
        (t t))

      (js-call *cursor* "scrollIntoView"
               (js-object "behavior" "smooth" "block" "center")))))

(defun element-next-sibling (x)
  (js-get x "nextElementSibling"))

(defun element-previous-sibling (x)
  (js-get x "previousElementSibling"))

(defun element-sibling (x direction)
  (ecase direction
    (:forward (element-next-sibling x))
    (:backward (element-previous-sibling x))))

(defun element-matches? (x selector)
  (js-call x "matches" selector))

(defun element-insert-adjacent! (x place y)
  (print (list :insert-adjacent x place y))
  (js-call x "insertAdjacentElement" (symbol-name place) y))

(defun element-closest (x selector)
  (js-call x "closest" selector))

(defun element-parent (x)
  (js-get x "parentElement"))

(defun forward! (direction into?)
  (let ((next (element-sibling *cursor* direction)))
    (if next
        (let ((place
                (if (element-matches? next "div, article, header, main")
                    (progn (print :matched)
                           (if (eq? direction :forward)
                               (if into? :afterbegin :afterend)
                             (if into? :beforeend :beforebegin)))
                  (if (eq? direction :forward)
                      :afterend
                    :beforebegin))))
          (returning t
            (element-insert-adjacent! next place *cursor*)))
      (let ((up (element-closest (element-parent *cursor*)
                                 "div, article, header, main"))
            (place (if (eq? direction :forward) :afterend :beforebegin)))
        (if (element-closest (element-parent up) "#file")
            (returning t
              (element-insert-adjacent! up place *cursor*))
          nil)))))

(defun goto-next-line! (direction)
  (when (element-closest (element-parent *cursor*)
                         "#file")
    (let* ((y0 (js-get *cursor* "offsetTop"))
           (y1 (and
                 (forward! direction t)
                 (js-get *cursor* "offsetTop"))))
      (when (eq? y0 y1)
        (goto-next-line! direction)))))

(defun select! (direction)
  (element-insert-adjacent! *cursor* :beforeend
                            (element-sibling *cursor* direction)))

(defun element-children (x)
  (js-get x "children"))

(defun element-insert-many-before! (x xs)
  (js-call-with-vector x "before" (element-children x)))

(defun unselect! ()
  (element-insert-many-before! *cursor*
                               (element-children *cursor*)))

(defun transpose! ()
  (let ((next (element-sibling *cursor* :forward))
        (prev (element-sibling *cursor* :backward)))
    (progn
      (element-insert-adjacent! prev :beforebegin next)
      (forward! :backward nil))))

(defun eval! (skip?)
  (let ((kids (element-children *cursor*)))
    (if (> (vector-length kids) 0)
        (vector-for-each kids #'eval-dexp!)
      (let ((next (element-sibling *cursor* :forward)))
        (when next
          (eval-dexp! next)
          (when skip?
            (forward! :forward nil)))))))

(defun save! ()
  (let ((code (js-call *wisp* "domCode" (query-selector "#file"))))
    (js-call (js-get *window* "localStorage") "setItem" "wisp-file" code)))

(defun eval-dexp! (x)
  (do-eval (read-from-string (js-call *wisp* "domCode" x))))

(defun element-remove! (x)
  (js-call x "remove"))

(defun element-replace-children! (x xs)
  (js-call-with-vector x "replaceChildren" xs))

(defun delete! ()
  (if (> (vector-length (element-children *cursor*)) 0)
      (element-replace-children! *cursor* [])
    (let ((next (element-sibling *cursor* :forward)))
      (when next
        (element-remove! next)))))

(defun element-deep-clone (x)
  (js-call x "cloneNode" t))

(defun duplicate! ()
  (let* ((next (element-sibling *cursor* :forward))
         (copy (element-deep-clone next)))
    (when next
      (element-insert-adjacent! next :beforebegin copy))))

(defun insert-code! (code)
  (let ((forms (read-many-from-string code)))
    (progn
      (element-replace-children! *cursor* [])
      (idom-patch! *cursor* *render-sexp-callback* forms)
      (unselect!))))

(defun start-editor! ()
  (js-call *wisp* "startEditor"
           (js-call *document* "querySelector" ".cursor")
           ""
           (vector-from-list
            (map #'symbol-name
                 (package-symbols (find-package "WISP"))))
           (make-pinned-value #'insert-code!)))

(defun do-render-sexp (forms)
  (with-simple-error-handler ()
    (for-each forms #'render-sexp)))

(defmacro note (date &rest notes)
  `(quote (note ,date ,@notes)))

(defun output (x)
  (set! *eval-output* (cons x *eval-output*))
  (idom-patch! (query-selector "#eval-output")
               *render-sexp-callback* *eval-output*))

(defun do-eval (expr)
  (with-simple-error-handler ()
    (set! *eval-output* (cons (eval expr) *eval-output*))
    (idom-patch!
     (query-selector "#eval-output")
     *render-sexp-callback* *eval-output*)))

(defun wisp-boot (forms)
  (with-simple-error-handler ()
    (dom-on-keydown! *key-callback*)
    (dom-on-window-event! "wisp-eval" (make-callback 'do-eval))
    (render-app (or forms *initial-forms*))))
