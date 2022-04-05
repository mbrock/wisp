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

(defun query-selector (selector)
  (js-call *document* "querySelector" selector))

(with-simple-error-handler ()
  (defvar *window* (js-global-this))
  (defvar *document* (js-get *window* "document"))
  (defvar *wisp* (js-get *window* "wisp"))
  (defvar <promise> (js-get *window* "Promise"))
  (defvar *eval-output* '())
  (defvar *root-element* (query-selector "#wisp-app"))
  (defvar *key-callback* (make-callback 'on-keydown))
  (defvar *insert-callback* (make-callback 'on-insert))
  (defvar *render-callback* (make-callback 'draw-app))
  (defvar *render-sexp-callback* (make-callback 'do-render-sexp))
  (defvar *cursor* nil))

(defun promise? (x)
  (if (and (eq? 'external (type-of x))
           (js-get x "then"))
      t
    nil))

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

(defun key-info-string (key-info)
  (with-vector-elements key-info (key ctrl shift alt meta repeat)
    (string-append (if ctrl "C-" "")
                   (if meta "M-" "")
                   (if shift "S-" "")
                   key)))

(defun keymap-select (key-info keymap)
  (let ((key-string (key-info-string key-info)))
    (call-with-prompt :break
        (fn ()
          (for-each keymap
            (fn (binding)
              (let ((keys (if (string? (head binding))
                              (list (head binding))
                            (head binding))))
                (for-each keys
                  (fn (candidate)
                    (when (string-equal? candidate key-string)
                      (print binding)
                      (send! :break (second binding)))))))))
      (fn (v k) v))))

(defmacro make-keymap (&rest clauses)
  `(list ,@(map (fn (clause)
                  (progn
                    (print `(list (quote ,(head clause))))
                    `(list ',(head clause) ,(second clause))
                    ))
                clauses)))

(defun forward-sexp! ()
  (forward! :forward nil))
(defun backward-sexp! ()
  (forward! :backward nil))
(defun forward-into-sexp! ()
  (forward! :forward t))
(defun backward-into-sexp! ()
  (forward! :backward t))
(defun select-forward-sexp! ()
  (select! :forward))
(defun select-backward-sexp! ()
  (select! :backward))
(defun forward-line! ()
  (goto-next-line! :forward))
(defun backward-line! ()
  (goto-next-line! :backward))
(defun evaluate-sexp! ()
  (eval! nil))

(defun on-keydown (key-info)
  (with-simple-error-handler ()
    (let ((function (keymap-select key-info *wisp-keymap*)))
      (if function
          (returning nil
            (progn
              (call function)
              (js-call *cursor* "scrollIntoView"
                       (js-object "behavior" "smooth"
                                  "block" "center"))))
        t))))

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

(defun render-eval-output ()
  (idom-patch!
   (query-selector "#eval-output")
   *render-sexp-callback* *eval-output*))

(defun do-eval (expr)
  (with-simple-error-handler ()
    (let* ((result (async (fn () (eval expr))))
           (thing (if (promise? result)
                      (let ((cell (vector 'pending result)))
                        (returning cell
                          (async (fn ()
                                   (print :awaiting-result)
                                   (vector-set! cell 1 (await result))
                                   (vector-set! cell 0 :done)
                                   (print (list :got cell))
                                   (log (vector-get cell 1))
                                   (render-eval-output)))))
                    result)))
      (set! *eval-output* (cons thing *eval-output*))
      (render-eval-output))))

(defvar *wisp-keymap*
  (make-keymap
   (("f" "ArrowRight") #'forward-sexp!)
   (("b" "ArrowLeft")  #'backward-sexp!)
   (("C-f" "C-ArrowRight") #'forward-into-sexp!)
   (("C-b" "C-ArrowLeft") #'backward-into-sexp!)
   (("S-F" "S-ArrowRight") #'select-forward-sexp!)
   (("S-B" "S-ArrowLeft") #'select-backward-sexp!)
   (("p" "ArrowUp") #'backward-line!)
   (("n" "ArrowDown") #'forward-line!)
   ("t" #'transpose!)
   ("k" #'delete!)
   ("d" #'duplicate!)
   (("C-g" "Escape") #'unselect!)
   ("i" #'start-editor!)
   ("e" #'evaluate-sexp!)
   ("s" #'save!)))

(defun wisp-boot (forms)
  (with-simple-error-handler ()
    (dom-on-keydown! *key-callback*)
    (render-app forms)))

(defun new-auth0-client ()
  (await (js-call *window* "createAuth0Client"
                  (js-object "domain" "dev-wnks73rd.us.auth0.com"
                             "client_id" "tJwSob2zIUMr0Di0sHM46CsYcLz70r10"))))

(defvar *auth0* nil)
(defvar *user* nil)

(defun auth0-login ()
  (set! *auth0* (new-auth0-client))
  (await (js-call *auth0* "loginWithPopup"))
  (set! *user* (await (js-call *auth0* "getUser")))
  (when *user*
    (vector "email" (js-get *user* "email")
            "name" (js-get *user* "name"))))

(defun auth0-get-token ()
  (await (js-call *auth0* "getTokenSilently")))
