;; -*- mode: wisp; fill-column: 64; -*-
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defvar *render-sexp-callback* (make-callback 'do-render-sexp))

(defun draw-app (forms)
  (tag :article '((:class "active"))
    (tag :main ()
      (tag :ins '((:class "cursor")) nil)
      (for-each forms #'render-sexp))
    (tag :header ()
      (tag :b ()
        (text "demo.wisp"))))
  (tag :article '((:class "output"))
    (tag :main ()
      (tag :ins '((:class "cursor")) nil))
    (tag :header ()
      (tag :i ()
        (text "*evaluation*")))))

(defun cursor ()
  (query-selector ".active .cursor"))

(defun output-buffer ()
  (query-selector "article.output > main"))

(defun create-element (tag-name)
  (js-call *document* "createElement" tag-name))

(defun create-text-node (text)
  (js-call *document* "createTextNode" text))

(defun set-inner-text! (element text)
  (js-set! element "innerText" text))

(defun render-sexp-to-element (sexp)
  (let ((div (create-element "DIV")))
    (do
      (idom-patch! div *render-sexp-callback* (list sexp))
      (query-selector ":scope > *" div))))

(defun render-sexp-to-html-string (sexp)
  (js-get (render-sexp-to-element sexp) "outerHTML"))

(defun render-app (forms)
  (with-simple-error-handler ()
    (idom-patch! (query-selector "#wisp-app")
                 (make-callback 'draw-app) forms)))

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
              (js-call (cursor) "scrollIntoView"
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
  (let ((next (element-sibling (cursor) direction)))
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
            (element-insert-adjacent! next place (cursor))))
      (let ((up (element-closest (element-parent (cursor))
                                 "div, article, section, header, main"))
            (place (if (eq? direction :forward) :afterend :beforebegin)))
        (if (element-closest (element-parent up) "main")
            (returning t
              (element-insert-adjacent! up place (cursor)))
          nil)))))

(defun goto-next-line! (direction)
  (when (element-closest (element-parent (cursor))
                         "#file")
    (let* ((y0 (js-get (cursor) "offsetTop"))
           (y1 (and
                 (forward! direction t)
                 (js-get (cursor) "offsetTop"))))
      (when (eq? y0 y1)
        (goto-next-line! direction)))))

(defun select! (direction)
  (element-insert-adjacent! (cursor) :beforeend
                            (element-sibling (cursor) direction)))

(defun element-children (x)
  (js-get x "children"))

(defun element-insert-many-before! (x xs)
  (js-call-with-vector x "before" (element-children x)))

(defun element-insert-many-after! (x xs)
  (js-call-with-vector x "after" (element-children x)))

(defun unselect! ()
  (element-insert-many-after! (cursor)
                              (element-children (cursor))))

(defun transpose! ()
  (let ((next (element-sibling (cursor) :forward))
        (prev (element-sibling (cursor) :backward)))
    (do
      (element-insert-adjacent! prev :beforebegin next)
      (forward! :backward nil))))

(defun eval! (skip?)
  (let ((kids (element-children (cursor))))
    (if (> (vector-length kids) 0)
        (vector-for-each kids #'eval-dexp!)
      (let ((next (element-sibling (cursor) :forward)))
        (when next
          (eval-dexp! next)
          (when skip?
            (forward! :forward nil)))))))

(defun dom-code (x)
  (js-call *wisp* "domCode" x))

(defun save! ()
  (js-call (js-get *window* "localStorage")
      "setItem" "wisp-file"
      (dom-code (query-selector "#file"))))

(defun eval-dexp! (x)
  (do-eval (read-from-string (dom-code x))))

(defun element-remove! (x)
  (js-call x "remove"))

(defun element-replace-children! (x xs)
  (js-call-with-vector x "replaceChildren" xs))

(defun delete! ()
  (if (> (vector-length (element-children (cursor))) 0)
      (element-replace-children! (cursor) [])
    (let ((next (element-sibling (cursor) :forward)))
      (when next
        (element-remove! next)))))

(defun element-deep-clone (x)
  (js-call x "cloneNode" t))

(defun duplicate! ()
  (let* ((next (element-sibling (cursor) :forward))
         (copy (element-deep-clone next)))
    (when next
      (element-insert-adjacent! next :beforebegin copy))))

(defun insert-code! (code)
  (let ((forms (read-many-from-string code)))
    (do
      (element-replace-children! (cursor) [])
      (idom-patch! (cursor) *render-sexp-callback* forms)
      (unselect!))))

(defun join-strings (separator strings)
  (cond ((nil? strings) "")
        ((nil? (tail strings)) (head strings))
        (t (string-append (head strings)
                          separator
                          (join-strings separator (tail strings))))))

(defun %list-from-vector (v i acc)
  (if (eq? (vector-length v) i)
      (reverse acc)
    (%list-from-vector v (+ i 1) (cons (vector-get v i) acc))))

(defun list-from-vector (v)
  (%list-from-vector v 0 ()))

(defun start-editor! ()
  (let* ((kids (list-from-vector (element-children (cursor))))
         (code (join-strings "\n" (map (fn (x)
                                         (print-to-string
                                          (read-from-string
                                           (dom-code x))))
                                       kids))))
    (element-replace-children! (cursor) [])
    (js-call *wisp* "startEditor"
      (cursor) code
      (vector-from-list
       (map #'symbol-name
            (package-symbols (find-package "WISP"))))
      (make-pinned-value #'insert-code!))))

(defun do-render-sexp (forms)
  (with-simple-error-handler ()
    (for-each forms #'render-sexp)))

(defmacro note (date &rest notes)
  `(quote (note ,date ,@notes)))

(defun display (value)
  (do-eval `(quote ,value)))

(defun do-eval (expr)
  (let ((result
          (try (async (fn ()
                        (try (eval expr)
                          (catch (e k)
                            (ktx-show k (list :🔥 e))))))
            (catch (e k)
              (ktx-show k (list :🔥 e))))))
    (let* ((thing
             (if (promise? result)
                 (list 'pending-promise result)
               result))
           (element (render-sexp-to-element thing)))
      (element-insert-adjacent! (output-buffer) :beforeend element)
      (when (promise? result)
        (async (fn ()
                 (let ((value (await result)))
                   (do
                     (log value)
                     (element-insert-adjacent!
                      element :afterend
                      (render-sexp-to-element
                       (list 'resolved-promise value)))
                     (element-remove! element))))))))

  (element-insert-adjacent! (output-buffer) :beforeend
                            (query-selector "ins" (output-buffer))))

(defvar *wisp-keymap* nil)

(defmacro set-keymap! (&rest clauses)
  `(set! *wisp-keymap* (make-keymap ,@clauses)))

(defun other-window! ()
  (let ((current-window (query-selector "article.active"))
        (other-window (query-selector "article:not(.active)")))
    (do (js-call (js-get current-window "classList") "toggle" "active" nil)
        (js-call (js-get other-window "classList") "toggle" "active" t))))

(defun goto-place-anywhere! ()
  (goto-place! t))

(defun goto-place-inside! ()
  (goto-place! nil))

(defun string-nth (s i)
  (string-slice s i (+ i 1)))

(defun radixify (alphabet m i)
  (if (< i m)
      (string-nth alphabet i)
    (string-append
     (radixify alphabet m (/ i m))
     (string-nth alphabet (mod i m)))))

(defun string-repeat (c n)
  (if (eq? n 0) ""
    (string-append c
                   (string-repeat c (- n 1)))))

(defun string-pad-left (s n x)
  (string-append (string-repeat x (- n (string-length s))) s))

(defun read-n-keys (n &optional acc)
  (if (eq? n 0)
      (join-strings "" (reverse acc))
    (read-n-keys (- n 1) (cons (vector-get (read-key) 0) acc))))

(defun goto-place! (anywhere?)
  (let* ((i 0)
         (element (if anywhere?
                      (element-closest (cursor) "article")
                    (element-parent (cursor))))
         (alphabet
           "0123456789abcdefghijklmnopqrstuvwxyz")
         (alphabet-size (string-length alphabet))
         (child-vector (query-selector-all ".list" element))
         (child-count (vector-length child-vector)))
    (when (> child-count 0)
      (let ((char-count
              (string-length
               (radixify alphabet alphabet-size (- child-count 1)))))
        (do
          (vector-for-each child-vector
            (fn (x)
              (let ((sticker (create-element "aside"))
                    (sticker-key (string-pad-left
                                    (radixify alphabet alphabet-size i)
                                    char-count
                                    (string-nth alphabet 0)
                                    )))
                (do
                  (set-inner-text! sticker sticker-key)
                  (js-set! (js-get x "dataset") "wispStickerKey" sticker-key)
                  (element-insert-adjacent! x :afterbegin sticker)
                  (set! i (+ i 1))))))
          (let* ((key (read-n-keys char-count) 0)
                 (match (query-selector (string-append "[data-wisp-sticker-key='" key "']"))))
            (vector-for-each
                (query-selector-all "aside" element)
              #'element-remove!)
            (vector-for-each
                (query-selector-all "[data-wisp-sticker-key]:not([data-wisp-sticker-key=\"\"])" *document*)
              (fn (x)
                (js-set! (js-get x "dataset") "wispStickerKey" "")))
            (when match
              (element-insert-adjacent! match :beforebegin (cursor)))))))))

(defun wisp-boot (forms)
  (with-simple-error-handler ()
    (dom-on-keydown! (make-callback 'on-keydown))
    (js-set! *document* "onclick"
             (make-pinned-value
              (fn (x)
                (unless (element-closest (js-get x "target") ".cm-editor")
                  (let ((target (element-closest (js-get x "target")
                                                 ".wisp.value")))
                    (if target
                        (do
                          (unselect!)
                          (element-insert-adjacent! target :beforebegin (cursor))
                          (element-insert-adjacent! (cursor) :afterbegin target))
                      (unselect!)))))))
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
  (await (js-call *auth0* "getTokenWithPopup"
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

(defun dwim! ()
  (let* ((next (element-sibling (cursor) :forward))
         (todos (query-selector-all "[data-symbol-name=TODO]" next))
         (dones (query-selector-all "[data-symbol-name=DONE]" next)))
    (vector-for-each todos
      (fn (x)
       (do
         (element-insert-adjacent! x :afterend (render-sexp-to-element 'done))
         (element-remove! x))))
    (vector-for-each dones
      (fn (x)
       (do
         (element-insert-adjacent! x :afterend (render-sexp-to-element 'todo))
         (element-remove! x))))))

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
 ("Tab" other-window!)
 ("Enter" dwim!))
