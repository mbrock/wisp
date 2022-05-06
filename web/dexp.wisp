;; -*- mode: wisp; fill-column: 64; -*-
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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

(defmacro tag (tag-symbol attrs &rest body)
  (let ((tag-name-var (fresh-symbol!)))
    `(let ((,tag-name-var (symbol-name ,tag-symbol)))
       (idom-open-start! ,tag-name-var)
       (for-each ,attrs
                 (fn (attr)
                     (idom-attr! (symbol-name (head attr))
                                 (second attr))))
       (idom-open-end!)
       ,@body
       (idom-close! ,tag-name-var))))

(defun text (text)
  (idom-text! text))

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
  `(:style ,(css clauses)))

(defvar *render-sexp-callback* (make-callback 'do-render-sexp))

(defun draw-app (forms)
  (tag :wisp-window-grid ()
    (tag :wisp-window '((:class "file active"))
      (tag :header ()
        (text "index.wisp"))
      (tag :main ()
        (tag :ins '((:class "cursor")) nil)
        (for-each forms #'render-sexp))
      )

    (tag :wisp-window '((:class "output"))
      (tag :header ()
        (tag :i ()
          (text "scratch: 1")))
      (tag :main ()
        (tag :ins '((:class "cursor")) nil)))

    ;; (tag :wisp-window '((:class "output"))
    ;;   (tag :header ()
    ;;     (tag :i ()
    ;;       (text "scratch: 2")))
    ;;   (tag :main ()
    ;;     (tag :ins '((:class "cursor")) nil)))

    ;; (tag :wisp-window '((:class "output"))
    ;;   (tag :header ()
    ;;     (tag :i ()
    ;;       (text "scratch: 3")))
    ;;   (tag :main ()
    ;;     (tag :ins '((:class "cursor")) nil)))

    )

  (tag :wisp-echo-area ()
       (tag :main ()
            (tag :ins '((:class "cursor")))))
  )

(defun cursor ()
  (query-selector ".active .cursor"))

(defun output-buffer ()
  (query-selector "wisp-window.output > main"))

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
  (with-simple-error-handler
      (fn ()
        (idom-patch! (query-selector "wisp-frame")
                     (make-callback 'draw-app) forms))))

(defun key-info-string (key-info)
  (with-vector-elements key-info (key ctrl shift alt meta repeat)
    (string-append (if ctrl "C-" "")
                   (if meta "M-" "")
                   (if alt "A-" "")
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
                (js-object ;; "behavior" "smooth"
                           "block" "nearest" "inline" "nearest"))))
      t)))

(defun on-keydown (key)
  (with-simple-error-handler
      (fn ()
          (async (fn ()
                     (call *key-handler* key))))))

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
    (let ((y0 (js-get (cursor) "offsetTop"))
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
  (let* ((repo-path (string-append "/" (repo-key)))
         (file-path (string-append repo-path "/index.wisp")))
    (save-file-code! file-path)
    (git-add! repo-path "index.wisp")
    (git-commit! repo-path "Wisp User" "user@wisp.town" "index.wisp")
    (git-push! repo-path
               (string-append "https://boat.whale-justice.ts.net/git/" (repo-key))
               "master")))

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
  (with-simple-error-handler
      (fn ()
          (for-each forms #'render-sexp))))

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

(defun select-window! (other-window)
  (let ((current-window (query-selector "wisp-window.active")))
    (do (js-call (js-get current-window "classList") "toggle" "active" nil)
        (js-call (js-get other-window "classList") "toggle" "active" t))))

(defun other-window! ()
  (select-window! (or (query-selector "wisp-window.active + *")
                      (query-selector "wisp-window:not(.active)"))))

(defun goto-place-anywhere! ()
  (goto-place! t))

(defun goto-place-inside! ()
  (goto-place! nil))

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
                 (match (query-selector
                         (string-append "[data-wisp-sticker-key='" key "']"))))
            (vector-for-each
                (query-selector-all "aside" element)
              #'element-remove!)
            (vector-for-each
                (query-selector-all
                 "[data-wisp-sticker-key]:not([data-wisp-sticker-key=\"\"])"
                 *document*)
              (fn (x)
                (js-set! (js-get x "dataset") "wispStickerKey" "")))
            (when match
              (element-insert-adjacent! match :beforebegin (cursor)))))))))


(defvar *fs* (js-get *window* "fs"))
(defvar *filesystem* (js-get *fs* "promises"))
(defvar *git* (js-get *window* "git"))
(defvar *git-http* (js-get *window* "git_http"))

(defun mkdir (dir)
  (await (js-call *filesystem* "mkdir" dir)))

(defun git-clone (dir url ref depth)
  (mkdir (string-append "/" dir))
  (await (js-call *git* "clone"
           (js-object "fs" *fs* "http" *git-http*
                      "dir" (string-append "/" dir)
                      "url" url
                      "ref" ref
                      "singleBranch" "true"
                      "depth" depth
                      "onProgress" (callback (info)
                                     (log info))))))

(defun git-add! (repo path)
  (js-call *git* "add"
    (js-object "fs" *fs*
               "dir" (string-append "/" repo)
               "filepath" path)))

(defun git-commit! (repo name email message)
  (js-call *git* "commit"
    (js-object "fs" *fs*
               "dir" (string-append "/" repo)
               "message" message
               "author" (js-object "name" name "email" email))))

(defun git-push! (dir url ref)
  (let ((bearer (string-append "Bearer " (auth0-get-token))))
    (await
     (js-call *git* "push"
       (js-object "fs" *fs*
                  "http" *git-http*
                  "dir" dir
                  "url" url
                  "ref" ref
                  "headers" (js-object "Authorization" bearer))))))

(defun stat (x) (await (js-call *filesystem* "stat" x)))

(defun read-file-code! (path)
  (await (js-call *filesystem* "readFile" path "utf8")))

(defun save-file-code! (path)
  (await (js-call *filesystem* "writeFile" path
                  (dom-code (query-selector ".file.active main"))
                  "utf8")))

(defun repo-key ()
  (let ((hash (js-get* *window* '("location" "hash"))))
    (if (equal? hash "")
        nil
      (string-slice hash 2 (string-length hash)))))

(defun wisp-boot (forms)
  (with-simple-error-handler
      (fn ()
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
        (async
         (fn ()
           (let ((repo-key (repo-key)))
             (if (nil? repo-key)
                 (render-app '())
               (let* ((has-clone?
                        (equal? :yes
                                (try (returning :yes
                                       (stat (string-append "/" repo-key)))
                                  (catch (e k) :no)))))
                 (unless has-clone?
                   (print 'cloning)
                   (git-clone repo-key
                              (string-append "https://boat.whale-justice.ts.net/git/" repo-key)
                              "master" 1))
                 (let ((file-code
                         (read-many-from-string
                          (try
                            (read-file-code!
                             (string-append "/" repo-key "/index.wisp"))
                            (catch (e k)
                              "(file-not-found \"index.wisp\")")))))
                   (render-app file-code))))))))))

(defun new-auth0-client ()
  (await (js-call *window* "createAuth0Client"
                  (js-object "domain" "dev-wnks73rd.us.auth0.com"
                             "client_id" "tJwSob2zIUMr0Di0sHM46CsYcLz70r10"))))

(defvar *auth0* nil)
(defvar *user* nil)

(defun auth0-login ()
  (set! *auth0* (new-auth0-client))
  (js-set! *window* "auth0" *auth0*)
  (await (js-call *auth0* "loginWithPopup"
           (js-object "audience" "https://api.wisp.town"
                      "scope" "create:repositories"
                      "prompt" "login")))
  (set! *user* (await (js-call *auth0* "getUser")))
  (log "user")
  (log *user*)
  (when *user*
    (vector "email" (js-get *user* "email")
            "name" (js-get *user* "name"))))

(defun login! () (auth0-login))

(defun auth0-get-token ()
  (when (not *user*)
    (login!))
  (await (js-call *auth0* "getTokenSilently"
           (js-object "audience" "https://api.wisp.town"
                      "scope" "create:repositories"))))

(defun api-request! (method path)
  (fetch path
         "method" method
         "headers"
         (js-object "Authorization"
                    (string-append "Bearer " (auth0-get-token)))))

(defun new-remote-repository! ()
  (response-text (api-request! "POST" "/git")))

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

(defun m-x ()
  (let ((echo-area (query-selector "wisp-echo-area")))
    (select-window! echo-area)
    (start-editor!)))

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
 ("Enter" dwim!)
 ("A-x" m-x)
 )
