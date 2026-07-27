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

(defun fourth (xs)
  (head (tail (tail (tail xs)))))

(defun render-button-widget (widget)
  (do
    (idom-open-start! "button")
    (idom-attr!
     "class"
     (if (fourth widget)
         "wisp-widget wisp-button selected"
       "wisp-widget wisp-button"))
    (idom-attr! "type" "button")
    (idom-prop! "onclick"
                (pin-widget-button-action
                 (third widget)))
    (idom-open-end!)
    (text (second widget))
    (idom-close! "button")))

(defun render-input-widget (widget)
  (do
    (idom-open-start! "input")
    (idom-attr! "class" "wisp-widget wisp-input")
    (idom-attr! "type" "search")
    (idom-attr! "placeholder" (second widget))
    (idom-attr! "value" (third widget))
    (idom-prop! "onclick" *stop-widget-click-pin*)
    (idom-prop! "onkeydown"
                (pin-widget-input-action
                 (fourth widget)))
    (idom-open-end!)
    (idom-close! "input")))

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
     (let ((callee (head sexp)))
       (cond
         ((eq? callee :button)
          (render-button-widget sexp))
         ((eq? callee :input)
          (render-input-widget sexp))
         (t
          (let ((tag-type
                  (cond
                    ((eq? callee :section) :section)
                    ((eq? callee :article) :article)
                    (t :div))))
            (tag tag-type
                 `((:class "wisp value list")
                   (:data-callee
                    ,(if (symbol? callee)
                         (string-append
                          (package-name
                           (symbol-package callee))
                          ":"
                          (symbol-name callee))
                       "")))
                 (render-list-contents sexp)))))))
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
(defvar *render-widget-sexp-callback*
  (make-callback 'do-render-widget-sexp))
(defvar *stop-widget-click-pin*
  (make-pinned-value
   (fn (event)
     (js-call event "stopPropagation"))))

(defun render-command-hint (hint)
  (tag :span '((:class "command-hint"))
    (tag :kbd () (text (head hint)))
    (text (second hint))))

(defun draw-app (forms)
  (tag :wisp-window-grid ()
    (tag :wisp-window '((:class "file active"))
      (tag :header ()
        (tag :strong '((:class "window-title"))
          (text "index.wisp"))
        (tag :span '((:class "window-kind"))
          (text "structure")))
      (tag :main ()
        (tag :ins '((:class "cursor")) nil)
        (for-each forms #'render-sexp))
      )

    (tag :wisp-window '((:class "output"))
      (tag :header ()
        (tag :strong '((:class "window-title"))
          (text "listener"))
        (tag :span '((:class "window-kind"))
          (text "scratch 1")))
      (tag :main ()
        (tag :ins '((:class "cursor")) nil)))
    )

  (tag :wisp-echo-area ()
    (tag :main ()
      (tag :ins '((:class "cursor")) nil))
    (tag :aside '((:class "command-hints"))
      (tag :span '((:class "mode-name"))
        (text "structure"))
      (for-each
       '(("f b" "move")
         ("C-f C-b" "enter")
         ("S-F S-B" "select")
         ("u" "out")
         ("i" "edit")
         ("e" "eval")
         ("t" "transpose")
         ("k" "delete")
         ("d" "duplicate")
         ("Enter" "dwim")
         ("Tab" "window")
         ("." "jump"))
       #'render-command-hint)))
  )

(defun cursor ()
  (query-selector ".active .cursor"))

(defun output-buffer ()
  (query-selector "wisp-window.output > main"))

(defun computed-style (selector property)
  (let ((element (query-selector selector)))
    (if element
        (js-call
         (js-call *window* "getComputedStyle" element)
         "getPropertyValue" property)
      (error 'element-not-found selector))))

(defun set-style! (selector property value)
  (let ((element (query-selector selector)))
    (if element
        (returning value
          (js-call (js-get element "style")
                   "setProperty" property value))
      (error 'element-not-found selector))))

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

(defun render-widget-sexp-to-element (sexp resume)
  (let ((div (create-element "DIV")))
    (do
      (idom-patch!
       div
       *render-widget-sexp-callback*
       (list resume sexp))
      (query-selector ":scope > *" div))))

(defun render-sexp-to-html-string (sexp)
  (js-get (render-sexp-to-element sexp) "outerHTML"))

(defun append-child! (parent child)
  (js-call parent "appendChild" child))

(defun element-with-class (tag-name class-name)
  (let ((element (create-element tag-name)))
    (returning element
      (js-set! element "className" class-name))))

(defun text-element (tag-name class-name contents)
  (let ((element (element-with-class tag-name class-name)))
    (returning element
      (set-inner-text! element contents))))

(defun button (label action)
  (let ((element (text-element "BUTTON" "restart" label)))
    (returning element
      (js-set! element "onclick"
        (make-pinned-value
         (fn (event)
           (call action)))))))

(defvar *interactive-condition-marker* (fresh-symbol!))
(defvar <websocket> (js-get *window* "WebSocket"))
(defvar *dev-socket* nil)
(defvar *dev-ticket-number* 0)
(defvar *dev-tickets* nil)
(defvar *wisp-host* (js-get *window* "wispHost"))

(defun download-system! ()
  (async
   (fn ()
     (let ((image
             (await
              (js-call *wisp-host* "download"))))
       (append-child!
        (output-buffer)
        (render-sexp-to-element
         (list
          'downloaded
          (js-get image "name")
          (list (js-get image "bytes") 'bytes))))))))

(defun fork-system! ()
  (async
   (fn ()
     (let ((system
             (await (js-call *wisp-host* "fork"))))
       (append-child!
        (output-buffer)
        (render-sexp-to-element
         (list
          'forked-system
          (js-get system "id")
          (list 'parent
                (js-get system "parentId"))
          (list 'heap-image
                (js-get system "bytes")
                'bytes))))))))

(defun send-system! (system-id source)
  (js-call *wisp-host* "evaluate" system-id source))

(defun install-host-controls! ()
  (unless (query-selector ".download-system")
    (let ((control
            (button "download" #'download-system!)))
      (do
       (js-set! control "className"
                "window-action download-system")
       (append-child!
        (query-selector "wisp-window.output > header")
        control))))
  (unless (query-selector ".fork-system")
    (let ((control (button "fork" #'fork-system!)))
      (do
       (js-set! control "className"
                "window-action fork-system")
       (append-child!
        (query-selector "wisp-window.output > header")
        control)))))

(defvar *system-browser-limit* 18)
(defvar *widget-pins* nil)
(defparameter *widget-resume* nil)

(defun take-list (n xs)
  (if (or (eq? n 0) (nil? xs))
      nil
    (cons (head xs)
          (take-list (- n 1) (tail xs)))))

(defun pin-widget-callback (callback)
  (let ((pin (make-pinned-value callback)))
    (do
      (set! *widget-pins*
            (cons pin *widget-pins*))
      pin)))

(defun resume-widget! (resume event)
  (let ((outcome
          (guard-interactively
           (fn () (call resume event)))))
    (if (interactive-condition? outcome)
        (display-condition-report! outcome)
      outcome)))

(defun pin-widget-button-action (action)
  (let ((resume *widget-resume*))
    (pin-widget-callback
     (fn (event)
       (do
         (js-call event "stopPropagation")
         (resume-widget! resume action))))))

(defun pin-widget-input-action (tag)
  (let ((resume *widget-resume*))
    (pin-widget-callback
     (fn (event)
       (when (equal? (js-get event "key") "Enter")
         (do
           (js-call event "stopPropagation")
           (js-call event "preventDefault")
           (resume-widget!
            resume
            (list
             tag
             (js-get
              (js-get event "target")
              "value")))))))))

(defun widget-button (label event &optional selected)
  (list
   :button
   label
   event
   selected))

(defun widget-input (placeholder value event-tag)
  (list
   :input
   placeholder
   value
   event-tag))

(defun system-browser-symbol-matches? (symbol query)
  (or (eq? (string-length query) 0)
      (string-search
       (string-to-uppercase (symbol-name symbol))
       (string-to-uppercase query))))

(defun system-browser-package-button (package current)
  (widget-button
   (package-name package)
   (list :package package)
   (eq? package current)))

(defun system-browser-symbol-button (symbol current)
  (widget-button
   (symbol-name symbol)
   (list :symbol symbol)
   (eq? symbol current)))

(defun system-browser-table-sexp (table)
  (list
   (js-get table "name")
   (js-get table "count")))

(defun system-browser-status-sexp (stats)
  (list
   'status
   (list
    'system
    (js-get stats "id")
    (list 'parent (js-get stats "parentId")))
   (list
    'heap
    (list 'era (js-get stats "era"))
    (list 'resident
          (js-get stats "residentBytes")
          'bytes)
    (list 'pins (js-get stats "pinCount"))
    (list 'roots (js-get stats "rootCount")))
   (cons
    'objects
    (map #'system-browser-table-sexp
         (list-from-vector
          (js-get stats "tables"))))))

(defun system-browser-symbol-detail (symbol)
  (if (nil? symbol)
      (list 'selection 'none)
    (let* ((function (symbol-function symbol))
           (value (guard-interactively
                   (fn () (eval symbol)))))
      (list
       'symbol
       symbol
       (list 'package
             (package-name (symbol-package symbol)))
       (if function
           (list
            'function
            (type-of function)
            (list 'calls
                  (function-call-count function)))
         (list 'function nil))
       (if (interactive-condition? value)
           (list
            'value
            (list
             'unavailable
             (interactive-condition-value value)))
         (list 'value value))))))

(defun system-browser-sexp (package symbol query)
  (let* ((stats (js-call *wisp-host* "inspect"))
         (all-symbols
          (package-symbols package))
         (matching-symbols
          (filter all-symbols
                  (fn (candidate)
                    (system-browser-symbol-matches?
                     candidate query))))
         (shown-symbols
          (take-list *system-browser-limit*
                     matching-symbols)))
    (list
     'system-browser
     (cons
      'packages
      (cons
       (list 'count
             (js-get stats "packageCount"))
       (map
        (fn (candidate)
          (system-browser-package-button
           candidate package))
        (reverse (packages)))))
     (cons
      'package
      (cons
       (package-name package)
       (cons
        (list
         'symbols
         (length all-symbols)
         (list 'matching
               (length matching-symbols))
         (list 'showing
               (length shown-symbols)))
        (cons
         (widget-input
          "filter symbols, then Enter"
          query
          :query)
         (map
          (fn (candidate)
            (system-browser-symbol-button
             candidate symbol))
          shown-symbols)))))
     (system-browser-symbol-detail symbol)
     (system-browser-status-sexp stats)
     (widget-button "refresh" (list :refresh)))))

(defun render-system-browser-view! (view resume)
  (let ((old-pins *widget-pins*))
    (do
      (set! *widget-pins* nil)
      (let* ((next
              (render-widget-sexp-to-element view resume))
             (current
              (query-selector
               ".list[data-callee='WISP:SYSTEM-BROWSER']")))
        (do
          (if current
              (js-call current "replaceWith" next)
            (append-child! (output-buffer) next))
          (for-each old-pins
                    #'release-pinned-value!))))))

(defun system-browser-widget (package symbol query)
  (let ((event
          (send!
           :widget
           (system-browser-sexp package symbol query))))
    (ecase (head event)
      (:package
       (system-browser-widget (second event) nil ""))
      (:symbol
       (system-browser-widget package
                              (second event)
                              query))
      (:query
       (system-browser-widget package
                              nil
                              (second event)))
      (:refresh
       (system-browser-widget package symbol query)))))

(defun start-system-browser-widget! ()
  (call-with-effect-handler
   :widget
   (fn ()
     (system-browser-widget
      (find-package "WISP") nil ""))
   (fn (view resume raise)
     (render-system-browser-view! view resume))))

(defun install-system-browser! ()
  (unless
      (query-selector
       ".list[data-callee='WISP:SYSTEM-BROWSER']")
    (start-system-browser-widget!)))

(defun guard-interactively (body)
  (try (call body)
    (catch (condition continuation)
      (list *interactive-condition-marker*
            condition continuation body))))

(defun interactive-condition? (value)
  (and (pair? value)
       (eq? (head value) *interactive-condition-marker*)))

(defun interactive-condition-value (report)
  (second report))

(defun interactive-condition-continuation (report)
  (third report))

(defun interactive-condition-retry (report)
  (head (tail (tail (tail report)))))

(defun find-dev-ticket (number tickets)
  (if (nil? tickets)
      nil
    (let ((entry (head tickets)))
      (if (eq? number (head entry))
          entry
        (find-dev-ticket number (tail tickets))))))

(defun dev-tickets ()
  (map
   (fn (entry)
     (list (head entry)
           (interactive-condition-value (second entry))))
   *dev-tickets*))

(defun install-dev-ticket! (report element)
  (set! *dev-ticket-number* (+ *dev-ticket-number* 1))
  (set! *dev-tickets*
        (cons (list *dev-ticket-number* report element)
              *dev-tickets*))
  (when element
    (append-child!
     (query-selector ".debugger-title" element)
     (text-element
      "SPAN" "debugger-ticket"
      (string-append
       "ticket "
       (print-to-string *dev-ticket-number*)))))
  *dev-ticket-number*)

(defun forget-dev-report! (report)
  (set! *dev-tickets*
        (remove-if
         (fn (entry)
           (eq? report (second entry)))
         *dev-tickets*)))

(defun take-dev-ticket! (number)
  (let ((entry (find-dev-ticket number *dev-tickets*)))
    (if entry
        (returning entry
          (set! *dev-tickets*
                (remove-if
                 (fn (candidate)
                   (eq? number (head candidate)))
                 *dev-tickets*)))
      (error 'no-such-dev-ticket number))))

(defun clear-dev-ticket-element! (entry)
  (let ((element (third entry)))
    (when element
      (element-remove! element))))

(defun use-dev-ticket! (number value)
  (let* ((entry (take-dev-ticket! number))
         (report (second entry)))
    (do
      (clear-dev-ticket-element! entry)
      (call (interactive-condition-continuation report) value))))

(defun retry-dev-ticket! (number)
  (let* ((entry (take-dev-ticket! number))
         (report (second entry)))
    (do
      (clear-dev-ticket-element! entry)
      (call (interactive-condition-retry report)))))

(defun abort-dev-ticket! (number)
  (let* ((entry (take-dev-ticket! number))
         (report (second entry)))
    (do
      (clear-dev-ticket-element! entry)
      (list 'aborted number
            (interactive-condition-value report)))))

(defun condition-name (condition)
  (let ((name
          (cond
            ((symbol? condition) condition)
            ((and (pair? condition)
                  (symbol? (head condition)))
             (head condition))
            ((and (eq? 'vector (type-of condition))
                  (> (vector-length condition) 0)
                  (symbol? (vector-get condition 0)))
             (vector-get condition 0))
            (t nil))))
    (if name (symbol-name name) "ERROR")))

(defun replace-element! (old new)
  (js-call old "replaceWith" new))

(defun outcome-element (outcome)
  (if (interactive-condition? outcome)
      (debugger-element outcome)
    (render-sexp-to-element outcome)))

(defun settle-outcome! (element outcome)
  (if (promise? outcome)
      (async
       (fn ()
         (let ((settled
                 (guard-interactively
                  (fn () (await outcome)))))
           (replace-element!
            element
            (if (interactive-condition? settled)
                (debugger-element settled)
              (render-sexp-to-element
               (list 'resolved-promise settled)))))))
    (replace-element! element (outcome-element outcome))))

(defun restart-condition! (element continuation value)
  (settle-outcome!
   element
   (guard-interactively
    (fn () (call continuation value)))))

(defun restart-condition-with-source!
    (element continuation report)
  (let ((source (js-call *window* "prompt"
                         "Lisp value to return from the condition:"
                         "nil")))
    (when source
      (forget-dev-report! report)
      (let ((value
              (guard-interactively
               (fn () (eval (read-from-string source))))))
        (if (interactive-condition? value)
            (settle-outcome! element value)
          (restart-condition! element continuation value))))))

(defun abort-condition! (element condition)
  (replace-element!
   element
   (render-sexp-to-element (list 'aborted condition))))

(defun retry-condition! (element body)
  (settle-outcome! element (guard-interactively body)))

(defun debugger-element (report)
  (let* ((condition (interactive-condition-value report))
         (continuation
           (interactive-condition-continuation report))
         (retry (interactive-condition-retry report))
         (debugger
           (element-with-class "ARTICLE" "wisp-debugger"))
         (header
           (element-with-class "HEADER" "debugger-title"))
         (condition-body
           (element-with-class "SECTION" "debugger-condition"))
         (restarts
           (element-with-class "NAV" "debugger-restarts"))
         (context
           (element-with-class "DETAILS" "debugger-context")))
    (do
      (append-child!
       header
       (text-element "SPAN" "debugger-kicker"
                     "unhandled condition"))
      (append-child!
       header
       (text-element "STRONG" "debugger-name"
                     (condition-name condition)))
      (append-child! debugger header)

      (append-child!
       condition-body
       (render-sexp-to-element
        (list 'condition condition)))
      (append-child! debugger condition-body)

      (append-child!
       restarts
       (button "use nil"
         (fn ()
           (forget-dev-report! report)
           (restart-condition! debugger continuation nil))))
      (append-child!
       restarts
       (button "supply value…"
         (fn ()
           (restart-condition-with-source!
            debugger continuation report))))
      (append-child!
       restarts
       (button "retry"
         (fn ()
           (forget-dev-report! report)
           (retry-condition! debugger retry))))
      (append-child!
       restarts
       (button "abort"
         (fn ()
           (forget-dev-report! report)
           (abort-condition! debugger condition))))
      (append-child! debugger restarts)

      (append-child!
       context
       (text-element "SUMMARY" "debugger-context-title"
                     "continuation"))
      (append-child!
       context
       (render-sexp-to-element
        (show-ktx continuation)))
      (append-child! debugger context)
      debugger)))

(defun display-condition-report! (report)
  (let ((home (or (output-buffer)
                  (query-selector "wisp-frame")))
        (element (debugger-element report)))
    (returning element
      (when home
        (element-insert-adjacent!
         home :beforeend element)))))

(defun eval-dev-forms (forms)
  (if (nil? forms)
      nil
    (let ((value (await (eval (head forms)))))
      (if (nil? (tail forms))
          value
        (eval-dev-forms (tail forms))))))

(defun send-dev-outcome! (socket outcome)
  (if (interactive-condition? outcome)
      (let ((ticket
              (install-dev-ticket!
               outcome
               (display-condition-report! outcome))))
        (js-call socket "send"
          (print-to-string
           (list :condition
                 ticket
                 (interactive-condition-value outcome)
                 (list :restarts
                       :use-value :retry :abort)))))
    (js-call socket "send"
      (print-to-string (list :ok outcome)))))

(defun eval-dev-source! (socket source)
  (let ((outcome
          (guard-interactively
           (fn ()
             (async
              (fn ()
                (eval-dev-forms
                 (read-many-from-string source))))))))
    (if (promise? outcome)
        (async
         (fn ()
           (send-dev-outcome!
            socket
            (guard-interactively
             (fn () (await outcome))))))
      (send-dev-outcome! socket outcome))))

(defun local-development-host? ()
  (let ((hostname
          (js-get (js-get *window* "location") "hostname")))
    (or (equal? hostname "127.0.0.1")
        (equal? hostname "localhost"))))

(defun schedule-dev-reconnect! ()
  (js-call *window* "setTimeout"
    (callback (ignored)
      (connect-dev-server!)
      nil)
    500))

(defun connect-dev-server! ()
  (when (and (local-development-host?)
             (nil? *dev-socket*))
    (let* ((location (js-get *window* "location"))
           (scheme
             (if (equal? (js-get location "protocol") "https:")
                 "wss://" "ws://"))
           (socket
             (new <websocket>
               (string-append scheme
                              (js-get location "host")
                              "/swank"))))
      (do
        (set! *dev-socket* socket)
        (js-set! socket "onopen"
          (callback (event)
            (js-call socket "send"
              (print-to-string
               (list :hello "browser wisp")))
            nil))
        (js-set! socket "onmessage"
          (callback (event)
            (eval-dev-source! socket
                              (js-get event "data"))
            nil))
        (js-set! socket "onclose"
          (callback (event)
            (set! *dev-socket* nil)
            (schedule-dev-reconnect!)
            nil))))))

(defun with-interactive-condition-handler (body)
  (let ((outcome (guard-interactively body)))
    (cond
      ((interactive-condition? outcome)
       (returning nil
         (display-condition-report! outcome)))
      ((promise? outcome)
       (async
        (fn ()
          (let ((settled
                  (guard-interactively
                   (fn () (await outcome)))))
            (if (interactive-condition? settled)
                (returning nil
                  (display-condition-report! settled))
              settled)))))
      (t outcome))))

(defun render-app (forms)
  (with-simple-error-handler
      (fn ()
        (do
         (idom-patch! (query-selector "wisp-frame")
                      (make-callback 'draw-app) forms)
         (install-host-controls!)
         (install-system-browser!)))))

(defun open-app! (forms)
  (do
    (render-app forms)
    (connect-dev-server!)))

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
  (with-interactive-condition-handler
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

(defun do-render-widget-sexp (data)
  (binding ((*widget-resume* (head data)))
    (render-sexp (second data))))

(defmacro note (date &rest notes)
  `(quote (note ,date ,@notes)))

(defun display (value)
  (do-eval `(quote ,value)))

(defun do-eval (expr)
  (let* ((result
           (guard-interactively
            (fn () (async (fn () (eval expr))))))
         (thing
           (if (promise? result)
               (list 'pending-promise result)
             result))
         (element (outcome-element thing)))
    (do
      (element-insert-adjacent! (output-buffer) :beforeend element)
      (when (promise? result)
        (async
         (fn ()
           (let ((value
                   (guard-interactively
                    (fn () (await result)))))
             (do
               (log value)
               (replace-element!
                element
                (if (interactive-condition? value)
                    (debugger-element value)
                  (render-sexp-to-element
                   (list 'resolved-promise value)))))))))
      (element-insert-adjacent! (output-buffer) :beforeend
                                (query-selector "ins"
                                                (output-buffer))))))

(defvar *wisp-keymap* nil)

(defmacro set-keymap! (&rest clauses)
  `(set! *wisp-keymap* (make-keymap ,@clauses)))

(defun select-window! (other-window)
  (let ((current-window (query-selector "wisp-window.active")))
    (do (js-call (js-get current-window "classList") "toggle" "active" nil)
        (js-call (js-get other-window "classList") "toggle" "active" t))))

(defun activate-window-containing! (element)
  (let ((window (element-closest element "wisp-window")))
    (when (and window
               (not (element-matches? window ".active")))
      (do
        (unselect!)
        (select-window! window)))))

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
  (with-interactive-condition-handler
      (fn ()
        (dom-on-keydown! (make-callback 'on-keydown))
        (js-set! *document* "onclick"
          (make-pinned-value
           (fn (x)
             (with-interactive-condition-handler
              (fn ()
                (let ((clicked (js-get x "target")))
                  (unless (element-closest clicked ".cm-editor")
                    (activate-window-containing! clicked)
                    (let ((target
                            (element-closest clicked ".wisp.value")))
                      (if target
                          (do
                            (unselect!)
                            (element-insert-adjacent!
                             target :beforebegin (cursor))
                            (element-insert-adjacent!
                             (cursor) :afterbegin target))
                        (unselect!))))))))))
        (async
         (fn ()
           (let ((repo-key (repo-key)))
             (if (nil? repo-key)
                 (open-app! forms)
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
                   (open-app! file-code))))))))))

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
