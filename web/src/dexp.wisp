;; -*- mode: wisp; fill-column: 64; -*-
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; * Basic binding to Incremental DOM

(defun make-callback (symbol)
  (dom-make-callback
   (package-name (symbol-package symbol))
   (symbol-name symbol)))

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

(defun-noexpand render-sexp (sexp)
  (cond
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
            (t (text (symbol-name sexp))))))
    ((pair? sexp)
     (tag :div '((:class "list"))
       (render-list-contents sexp)))
    ((eq? 'string (type-of sexp))
     (tag :span '((:class "string"))
       (text sexp)))
    ((eq? 'vector (type-of sexp))
     (tag :div '((:class "vector"))
       (render-vector-contents sexp 0)))))

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

(defun draw-app ()
  (tag :style () (text "
    #wisp-app { margin: 10px; }

    .symbol { text-transform: lowercase; }

    .string { font-family: 'DM Mono', monospace; }
    .string:before { content: '“'; }
    .string:after { content: '”'; }

    [data-function-kind=jet] { color: goldenrod; }
    [data-function-kind=fun] { color: lightsalmon; }

    .vector, .list {
      display: flex; flex-wrap: wrap; align-items: center;
      gap: 5px; margin: 0 5px; padding: 0 5px;
      max-width: 60ch;
      min-height: 1em;
    }

    .list { border-color: #555; border-width: 0 2px; border-radius: 10px; }
    .vector { border-color: #558; border-width: 1px; }

    .cursor:empty { height: 5px; width: 5px; border-radius: 100%; }
    .cursor { background: #63ffeb80; }
    .cursor { display: flex; flex-wrap: wrap; align-items: center; gap: 5px; }

    .cursor:not(:empty) { background: #63ffeb40; border-radius: 5px; }
    .cursor:not(:empty) { margin: 0 5px; padding: 0 5px; }
  "))
  (tag :span '((:class "cursor")) nil)
  (render-sexp `(defun render-sexp (sexp)
                  ,(code #'render-sexp)))
  (render-sexp
   '(note "March 29, 2022"
     (chores
      (done "implement structural editor")
      (done "play around")
      (todo "buy bananas"))))

  (render-sexp
   '(defun toggle (item)
     (cond
       ((eq? item 'todo) 'done)
       ((eq? item 'done) 'todo))))

  (progn
    (set! *cursor-element* (query-selector ".cursor"))
    (dom-cursor-step! *cursor-element* 0 nil nil)
    (print (list :cursor-element *cursor-element*))))

(defvar *buffer* ())
(defvar *root-element* (query-selector "#wisp-app"))
(defvar *key-callback* (make-callback 'on-keydown))
(defvar *render-callback* (make-callback 'draw-app))
(defvar *cursor-element* nil)

(print (list 'root *root-element*
             'key-callback *key-callback*
             'render-callback *render-callback*))

(defun render-app ()
  (with-simple-error-handler ()
    (idom-patch! *root-element* *render-callback* nil)))

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
             (string-equal? key "f")
             (string-equal? key "F"))
         (dom-cursor-step! *cursor-element* 0 control? shift?))
        ((or (string-equal? key "ArrowLeft")
             (string-equal? key "b")
             (string-equal? key "B"))
         (dom-cursor-step! *cursor-element* 1 control? shift?))
        ((or (string-equal? key "ArrowUp")
             (string-equal? key "p"))
         (dom-cursor-step! *cursor-element* 2 nil nil))
        ((or (string-equal? key "ArrowDown")
             (string-equal? key "n"))
         (dom-cursor-step! *cursor-element* 3 nil nil))
        ((string-equal? key "t")
         (dom-cursor-step! *cursor-element* 4 nil nil))
        ((string-equal? key "k")
         (dom-cursor-step! *cursor-element* 5 nil nil))
        ((string-equal? key "d")
         (dom-cursor-step! *cursor-element* 6 nil nil))
        ((string-equal? key "Escape")
         (dom-cursor-step! *cursor-element* 7 nil nil))))))

(with-simple-error-handler ()
  (dom-on-keydown! *key-callback*)
  (render-app))
