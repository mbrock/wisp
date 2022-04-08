;; -*- mode: wisp; fill-column: 64; -*-
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
           `((:class "list")
             (:data-callee
              ,(if (symbol? callee)
                   (string-append
                    (package-name (symbol-package callee))
                    ":"
                    (symbol-name callee))
                 "")))
           (render-list-contents sexp))))
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
          (do
            (tag :span '((:class "dot"))
              (text "·"))
            (render-sexp tail))))))

  (defun render-vector-contents (vector i)
    (vector-each vector #'render-sexp))

  (defun style (clauses)
    `(:style ,(css clauses))))

(defvar *wisp* (js-get *window* "wisp"))
(defvar *root-element* (query-selector "#wisp-app"))
(defvar *cursor* nil)
(defvar *eval-output* '())

(defvar *key-callback* (make-callback 'on-keydown))
(defvar *insert-callback* (make-callback 'on-insert))
(defvar *render-callback* (make-callback 'draw-app))
(defvar *render-sexp-callback* (make-callback 'do-render-sexp))

(:section (wisp editor)
  (defun draw-app (forms)
    (tag :article '()
      (tag :main ()
        (tag :div
          `((:id "file")
            ,(style '((display inline-flex)
                      (flex-direction column)
                      (gap 15))))
          (tag :ins '((:class "cursor")) nil)
          (for-each forms #'render-sexp)))
      (tag :header `((:id "eval-output")
                     ,(style '((display "flex")
                               (flex-direction "column")
                               (gap 15))))
        nil))

    (do
      (idom-patch!
       (query-selector "#eval-output")
       *render-sexp-callback* *eval-output*)
      (set! *cursor* (query-selector ".cursor"))
      (print (list :cursor *cursor*)))))

(defun create-element (tag-name)
  (js-call *document* "createElement" tag-name))

(defun create-text-node (text)
  (js-call *document* "createTextNode" text))

(defun set-inner-text! (element text)
  (js-set! element "innerText" text))

(defun render-sexp-to-html-string (sexp)
  (let ((div (create-element "DIV")))
    (do
     (idom-patch! div *render-sexp-callback* (list sexp))
     (js-get div "innerHTML"))))

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
                      (send! :break (second binding)))))))))
      (fn (v k) v))))

(defmacro make-keymap (&rest clauses)
  `(list ,@(map (fn (clause)
                  `(list ',(head clause) ',(second clause)))
                clauses)))

(defun forward-sexp! ()
  (forward! :forward nil))
(defun backward-sexp! ()
  (forward! :backward nil))
(defun up-sexp! ()
  (forward! :backward nil :up))
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

(defvar *key-handler*
  (fn (x) (use-keymap x)))

(defun use-keymap (key)
  (let ((function-name (keymap-select key *wisp-keymap*)))
    (if function-name
        (returning nil
          (do (call (symbol-function function-name))
              (js-call *cursor* "scrollIntoView"
                (js-object "behavior" "smooth"
                           "block" "center"))))
      t)))

(defun on-keydown (key)
  (with-simple-error-handler ()
    (async (fn ()
             (call *key-handler* key)))))

(defun read-key ()
  (let ((old-key-handler *key-handler*))
    (await (new <promise>
                (make-pinned-value
                 (fn (ok)
                   (returning nil
                     (set! *key-handler*
                           (fn (key)
                             (let ((key-name (vector-get key 0)))
                               (when (not (or (equal? key-name "Meta")
                                              (equal? key-name "Shift")
                                              (equal? key-name "Control")
                                              (equal? key-name "Alt")))
                                 (returning
                                     (js-call-function ok key)
                                   (set! *key-handler* old-key-handler)))))))))))))

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

(defun forward! (direction into? &optional up?)
  (let ((next (element-sibling *cursor* direction)))
    (if (and next (not up?))
        (let ((place
                (if (element-matches? next "div, article, section, header, main")
                    (do (print :matched)
                           (if (eq? direction :forward)
                               (if into? :afterbegin :afterend)
                             (if into? :beforeend :beforebegin)))
                  (if (eq? direction :forward)
                      :afterend
                    :beforebegin))))
          (returning t
            (element-insert-adjacent! next place *cursor*)))
      (let ((up (element-closest (element-parent *cursor*)
                                 "div, article, section, header, main"))
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
    (do
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
    (do
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
  (try
    (with-simple-error-handler ()
      (let* ((result (async (fn () (eval expr))))
             (thing (if (promise? result)
                        (let ((cell (list 'pending-promise result)))
                          (returning cell
                            (async (fn ()
                                     (set-tail! cell (list (await result)))
                                     (set-head! cell 'resolved-promise)
                                     (render-eval-output)))))
                      result)))
        (set! *eval-output* (cons thing *eval-output*))
        (render-eval-output)))
    (catch (e k)
      (set! *eval-output* (cons e *eval-output*))
      (render-eval-output))))

(defvar *wisp-keymap* nil)

(defmacro set-keymap! (&rest clauses)
  `(set! *wisp-keymap* (make-keymap ,@clauses)))

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
 ("C-." goto-place-inside!))

(defun goto-place-anywhere! ()
  (goto-place! t))

(defun goto-place-inside! ()
  (goto-place! nil))

(defun goto-place! (anywhere?)
  (let* ((i 0)
         (element (if anywhere? *document* (element-parent *cursor*)))
         (alphabet
           "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (child-vector (query-selector-all ".list" element)))
    (when (> (vector-length child-vector) 0)
      (do
        (vector-for-each child-vector
          (fn (x)
            (let ((sticker (create-element "aside")))
              (do
                (set-inner-text! sticker (string-slice alphabet i (+ i 1)))
                (element-insert-adjacent! x :afterbegin sticker)
                (set! i (+ i 1))))))
        (let* ((key (vector-get (read-key) 0))
               (j (string-search alphabet key)))
          (vector-for-each
              (query-selector-all "aside" element)
            #'element-remove!)
          (when (and j (< j (vector-length child-vector)))
            (element-insert-adjacent!
             (vector-get child-vector j)
             :afterbegin
             *cursor*)))))))

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
  (await (js-call *auth0* "loginWithPopup"
                  (js-object "audience" "https://api.wisp.town"
                             "scope" "create:repositories")))
  (set! *user* (await (js-call *auth0* "getUser")))
  (when *user*
    (vector "email" (js-get *user* "email")
            "name" (js-get *user* "name"))))

(defun auth0-get-token ()
  (when (not *user*)
    (auth0-login))
  (await (js-call *auth0* "getTokenSilently"
                  (js-object "audience" "https://api.wisp.town"
                             "scope" "create:repositories"))))

(defun town-request (method path)
  (fetch (string-append "http://localhost:8000" path)
         "method" method
         "headers"
         (js-object "Authorization"
                    (string-append "Bearer " (auth0-get-token)))))

(defun town-create-repo ()
  (response-text (town-request "POST" "/git")))
