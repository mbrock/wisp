;; -*- mode: wisp; fill-column: 64; -*-
;;
;; This file is part of Wisp.
;;
;; Wisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version
;; 3 of the License, or (at your option) any later version.
;;
;; Wisp is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General
;; Public License along with Wisp. If not, see
;; <https://www.gnu.org/licenses/>.
;;



;;; DEFUN, DEFMACRO, DEFVAR

(set-symbol-function!
 'DEFUN
 (%macro-fn
  (name args &rest body)
  (list 'set-symbol-function! (list 'quote name)
        (list '%fn name args (prognify body)))))

(set-symbol-function!
 'DEFMACRO
 (%macro-fn
  (name args &rest body)
  (list 'set-symbol-function! (list 'quote name)
        (list '%macro-fn args (prognify body)))))

(defmacro defvar (var val)
  (list 'set-symbol-value! (list 'quote var) val))



;;; Backquote

(defvar *bq-quote* '#:bq-quote)
(defvar *bq-list* '#:bq-list)
(defvar *bq-append* '#:bq-append)

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens raw-result)))

(defun bq-loop (p q)
  (if (atom? p)
      (cons *bq-append*
            (reverse-append q (list (list *bq-quote* p))))
      (cond ((eq? (head p) 'unquote)
             (if (null (tail (tail p)))
                 (cons *bq-append*
                       (reverse-append q (list (head (tail p)))))
                 (error "malformed ,")))
            ((eq? (head p) 'unquote-splicing)
             (error "dotted ,@"))
            (t
             (bq-loop (tail p)
                      (cons (bracket (head p)) q))))))

(defun bq-process (x)
  (cond ((atom? x)
         (list *bq-quote* x))
        ((eq? (head x) 'backquote)
         (bq-process (bq-completely-process (head (tail x)))))
        ((eq? (head x) 'unquote)
         (head (tail x)))
        ((eq? (head x) 'unquote-splicing)
         (error ",@ after `"))
        (t (bq-loop x nil))))

(defun bracket (x)
  (cond ((atom? x)
         (list *bq-list* (bq-process x)))
        ((eq? (head x) 'unquote)
         (list *bq-list* (head (tail x))))
        ((eq? (head x) 'unquote-splicing)
         (head (tail x)))
        (t (list *bq-list* (bq-process x)))))

(defun maptree (fn x)
  (if (atom? x)
      (call fn x)
      (let ((a (call fn (head x)))
            (d (maptree fn (tail x))))
        (if (and (eq? a (head x))
                 (eq? d (tail x)))
            x
            (cons a d)))))

(defun bq-remove-tokens (x)
  (cond ((eq? x *bq-list*) 'list)
        ((eq? x *bq-append*) 'append)
        ((eq? x *bq-quote*) 'quote)
        ((atom? x) x)
        (t (maptree #'bq-remove-tokens x))))

(defmacro backquote (x)
  (bq-completely-process x))



;;; Standard library

(defun not (x)
  (if x nil t))

(defun atom? (x)
  (not (eq? 'cons (type-of x))))

(defun %cond (clauses)
  (if (nil? clauses) nil
      (let ((x (head clauses)))
        (list 'if
              (head x)
              (head (tail x))
              (%cond (tail clauses))))))

(defmacro cond (&rest clauses)
  (%cond clauses))

(defmacro and-2 (x y)
  (list 'if x y x))

(defmacro or-2 (x y)
  (list 'if x x y))

(defun nil? (x)
  (eq? x nil))

(defun last (xs)
  (if (nil? xs) nil
      (if (nil? (tail xs)) xs
          (last (tail xs)))))

(defun second (xs)
  (head (tail xs)))

(defun reduce-loop (f x xs)
  (cond ((nil? xs) x)
        ((nil? (tail xs))
         (call f x (head xs)))
        (t
         (reduce-loop f
                      (call f x (head xs))
                      (tail xs)))))

(defun reduce (f xs init)
  ;; TODO: version that ignores init when (length xs) is 2
  (reduce-loop f init xs))

(defun append-2 (xs ys)
  (if (nil? xs) ys
      (cons (head xs)
            (append (tail xs) ys))))

(defun append (&rest xss)
  (reduce #'append-2 xss '()))

(defmacro and (&rest xs)
  (reduce #'and-2 xs t))

(defmacro or (&rest xs)
  (reduce #'or-2 xs nil))

(defun snoc (x y)
  (cons y x))

(defun reverse (list)
  (reduce #'snoc list nil))

(defun reverse-append (list tail)
  (reduce #'snoc list tail))

(defmacro fn (params &rest body)
  `(%fn nil ,params ,(prognify body)))

(defun for-each (xs f)
  (if (nil? xs) nil
      (do
        (call f (head xs))
        (for-each (tail xs) f))))

(defmacro when (test &rest body)
  `(if ,test ,(prognify body) nil))

(defun pair? (x)
  (eq? 'cons (type-of x)))



;;; * Macroexpansion
;;;
;;; We define a code walker that can expand macros recursively.
;;;
;;; Unfortunately, this makes recursive macros loop forever.
;;; Maybe a recursion limit would be a reasonable way to
;;; solve that.

(defun iterative-fixpoint (f x)
  (let ((y (call f x)))
    (if (eq? x y) x
      (iterative-fixpoint f y))))

(defun macroexpand-1x (form)
  (if (pair? form)
      (let ((callee (head form)))
        (if (symbol? callee)
            (let ((function (symbol-function callee)))
              (let ((function-type (type-of function)))
                (cond ((and (eq? function-type 'function)
                            (not (jet-ctl? function)))
                       (maptree #'macroexpand-1x form))
                      ((eq? function-type 'macro)
                       (apply function (tail form)))
                      (t form))))
            form))
      form))

(defun macroexpand (form)
  (iterative-fixpoint #'macroexpand-1x form))

(defun macroexpand-completely (form)
  (iterative-fixpoint #'macroexpand-recursively form))

(defun macroexpand-recursively (form)
  (if (atom? form) form
      (let ((head (head form)))
        (cond
          ((or (eq? head 'if)
               (eq? head 'do))
           (maptree #'macroexpand-completely form))
          ((eq? head 'let)
           (let ((bindings (second form))
                 (body (tail (tail form))))
             (let ((bindings-expansion
                     (maptree
                      (fn (binding)
                        (let ((x (macroexpand-completely
                                  (head (tail binding)))))
                          (if (eq? x (head (tail binding)))
                              binding
                              (list (head binding) x))))
                      bindings))
                   (body-expansion
                     (maptree #'macroexpand-completely body)))
               (if (and (eq? bindings bindings-expansion)
                        (eq? body body-expansion))
                   form
                   (cons 'let (cons bindings-expansion body-expansion))))))
          ((eq? head 'quote) form)
          ((eq? head 'backquote) (bq-completely-process (second form)))
          ((eq? head '%fn)
           (let ((params (second form))
                 (body (head (last form))))
             (let ((body-expansion
                     (macroexpand-completely body)))
               (if (eq? body body-expansion)
                   form
                   `(%fn ,params ,body-expansion)))))
          (t
           (let ((expansion (macroexpand form)))
             (if (eq? form expansion)
                 form
                 (macroexpand-completely expansion))))))))

;;; Now we redefine DEFUN to use macroexpansion.
(defmacro defun (name args &rest body)
  (print (list 'defun name args))
  (let ((expanded-body
          (macroexpand-completely
           (prognify body))))
    `(set-symbol-function! ',name (%fn ,name ,args ,expanded-body))))

;;; We can also mutate the code of a function or macro.
(defun compile! (function)
  (print (list 'compiling (function-name function)))
  (set-code! function (macroexpand-completely (code function))))

;;; Now we can go back and compile everything in the package.
(defun compile-many! (package)
  (for-each (reverse (package-symbols package))
    (fn (symbol)
      (let ((function (symbol-function symbol)))
        (when (and function
                   (not (jet? function)))
          (compile! function))))))

(compile-many! (find-package "WISP"))

(for-each (reverse (package-symbols (find-package "WISP")))
          (fn (symbol)
              (let ((function (symbol-function symbol)))
                (when (and function
                           (not (jet? function))
                           (eq? 0 (function-call-count function)))
                  (print (list symbol
                               "can be moved to below COMPILE-MANY!"))))))



(defmacro defvar (var val)
  `(do
    (print (list 'defvar ',var))
    (set-symbol-value! ',var ,val)))

(defmacro defun-noexpand (name args &rest body)
  (print (list 'defun name args))
  `(set-symbol-function! ',name (%fn ,name ,args ,(prognify body))))

(defun list? (x)
  (or (pair? x) (nil? x)))

(defun string? (x)
  (eq? 'string (type-of x)))

(defun integer? (x)
  (eq? 'integer (type-of x)))

(defun vector? (x)
  (eq? 'vector (type-of x)))

(defun equal? (x y)
  (if (eq? x y) t
    (let ((xt (type-of x))
          (yt (type-of y)))
      (if (not (eq? xt yt))
          nil
        (if (eq? xt 'cons)
            (equal-lists? x y)
          (if (eq? xt 'string)
              (string-equal? x y)
            (eq? x y)))))))

(defun equal-lists? (x y)
  (if (eq? x nil)
      (eq? y nil)
      (if (eq? y nil)
          nil
          (if (equal? (head x) (head y))
              (equal-lists? (tail x) (tail y))
              nil))))

(defun map (f xs)
  (if (nil? xs) nil
      (cons (call f (head xs))
            (map f (tail xs)))))

(defun third (xs)
  (head (tail (tail xs))))

(defun length-aux (xs n)
  (if (nil? xs) n
      (length-aux (tail xs) (+ n 1))))

(defun length (xs)
  (length-aux xs 0))

(defun remove-if (f xs)
  (if (nil? xs) nil
      (if (call f (head xs))
          (remove-if f (tail xs))
          (cons (head xs) (remove-if f (tail xs))))))

(defun some? (f xs)
  (if (nil? xs) nil
      (or (call f (head xs))
          (some? f (tail xs)))))

(defun butlast (xs)
  (if (nil? xs) nil
      (if (nil? (tail xs)) nil
          (cons (head xs)
                (butlast (tail xs))))))

(defmacro unless (test &rest body)
  `(if ,test nil ,(prognify body)))

(defun %case->cond (thing clauses)
  (if (nil? clauses) nil
      (cons `((eq? ,thing ',(head (head clauses)))
              ,(prognify (tail (head clauses))))
            (%case->cond thing (tail clauses)))))

(defmacro ecase (thing &rest clauses)
  (let ((thing-variable (fresh-symbol!)))
    `(let ((,thing-variable ,thing))
       (cond
         ,@(%case->cond thing-variable clauses)
         (t (error 'case-fail ,thing-variable))))))

(defmacro set! (var val)
  (list '%set! (list 'quote var) val))

(defmacro assert (x)
  `(if ,x nil (error ',x)))

(defmacro returning (x &rest body)
  (let ((x-var (fresh-symbol!)))
    `(let ((,x-var ,x))
       (do ,@body ,x-var))))

(defmacro with-trace (&rest body)
  `(do
     (wtf t)
     (returning ,(prognify body)
       (wtf nil))))

(defun %let* (clauses body)
  (if (nil? clauses)
      body
      (let ((var (head (head clauses)))
            (exp (second (head clauses))))
        `(let ((,var ,exp))
           ,(%let* (tail clauses) body)))))

(defmacro let* (clauses &rest body)
  (%let* clauses (prognify body)))

(defun fix (f)
  (call f f))

(defun vector-for-each (xs f)
  (%vector-for-each xs f 0))

(defun %vector-for-each (xs f i)
  (when (< i (vector-length xs))
    (call f (vector-get xs i))
    (%vector-for-each xs f (+ i 1))))

(defun vector-for-each-backwards (xs f)
  (%vector-for-each xs f (vector-length xs) nil))

(defun %vector-for-each-backwards (xs f i)
  (when (< i (vector-length xs))
    (call f (vector-get xs i))
    (%vector-for-each xs f (+ i 1))))



;;; * Utilities for delimited continuation control

(defun send-or-invoke (tag value function)
  (let* ((default (fresh-symbol!))
         (result (send-with-default! tag value default)))
    (if (eq? result default)
        (call function value)
        result)))

(defun send-to-or-invoke (continuation tag value function)
  (let* ((default (fresh-symbol!))
         (result (send-to-with-default! continuation tag value default)))
    (if (eq? result default)
        (call function value)
        result)))

(defun error (&rest xs)
  (send-or-invoke 'error xs #'unhandled-error))

(defun nonlocal-error! (continuation &rest xs)
  (send-to-or-invoke continuation 'error xs #'unhandled-error))

(defun send! (tag &optional value)
  (send-or-invoke tag value
                  (fn (x)
                    (error 'prompt-tag-missing tag))))

(defmacro handle (body clause)
  (let ((tag-name (head clause))
        (handler-args (head (tail clause)))
        (handler-body (tail (tail clause))))
    `(call-with-prompt ',tag-name
         (fn () ,body)
       (fn ,handler-args ,@handler-body))))

(defmacro try (body clause)
  (let ((catch (head clause))
        (handler-args (head (tail clause)))
        (handler-body (tail (tail clause))))
    `(call-with-prompt 'error
         (fn () ,body)
       (fn ,handler-args ,@handler-body))))

(defmacro defparameter (parameter value)
  `(do (defvar ,parameter ,value)
       (set-symbol-dynamic! ',parameter t)))

(defun %binding (clauses body)
  (if (nil? clauses) `(do ,@body)
    (let ((clause (head clauses)))
      `(call-with-binding ',(head clause) ,(second clause)
                          (fn ()
                            ,(%binding (tail clauses) body))))))

(defmacro binding (clauses &rest body)
  (%binding clauses body))



(defun split-string (string separator &optional acc)
  (let ((idx (string-search string separator)))
    (if idx
        (split-string
         (string-slice string
                        (+ idx (string-length separator))
                        (string-length string))
         separator
         (cons (string-slice string 0 idx) acc))
        (reverse (cons string acc)))))

(defun join-strings (separator strings)
  (cond ((nil? strings) "")
        ((nil? (tail strings)) (head strings))
        (t (string-append (head strings)
                          separator
                          (join-strings separator (tail strings))))))

(defun string-nth (s i)
  (string-slice s i (+ i 1)))

(defun string-repeat (c n)
  (if (eq? n 0) ""
    (string-append c
                   (string-repeat c (- n 1)))))

(defun string-pad-left (s n x)
  (string-append (string-repeat x (- n (string-length s))) s))

(defun radixify (alphabet m i)
  (if (< i m)
      (string-nth alphabet i)
    (string-append
     (radixify alphabet m (/ i m))
     (string-nth alphabet (mod i m)))))

(defun %list-from-vector (v i acc)
  (if (eq? (vector-length v) i)
      (reverse acc)
    (%list-from-vector v (+ i 1) (cons (vector-get v i) acc))))

(defun list-from-vector (v)
  (%list-from-vector v 0 ()))

(defun vector-each (vector function)
  (vector-each-loop vector function 0))

(defun vector-each-loop (vector function i)
  (when (< i (vector-length vector))
    (call function (vector-get vector i))
    (vector-each-loop vector function (+ i 1))))

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

(defun remove (list element)
  (if (nil? list) nil
    (let ((head (head list)))
      (if (equal? head element)
          (tail list)
        (cons head (remove (tail list) element))))))

(defun find (list predicate)
  (if (nil? list)
      nil
    (let ((x (head list)))
      (if (call predicate x)
          (cons x nil)
        (find (tail list) predicate)))))

(defun includes? (list item)
  (find list (fn (x) (equal? x item))))

(defun find-result (list predicate)
  (if (nil? list)
      nil
    (let* ((x (head list))
           (result (call predicate x)))
      (if result
          (cons x result)
        (find-result (tail list) predicate)))))

(defun filter (list predicate &optional acc)
  (if (nil? list)
      (reverse acc)
    (let* ((x (head list))
           (next-acc (if (call predicate x)
                         (cons x acc)
                       acc)))
      (filter (tail list) predicate next-acc))))

(defun %defpackage-clause (pkg-sym clause)
  (ecase (head clause)
    (:use `(package-set-uses!
            ,pkg-sym
            (list ,@(map (fn (name)
                           `(find-package ,(symbol-name name)))
                         (tail clause)))))))

(defun %%defpackage (name clauses)
  (let ((pkg-sym (fresh-symbol!)))
    `(let ((,pkg-sym (%defpackage ,(symbol-name name))))
       (do ,@(map (fn (clause)
                    (%defpackage-clause pkg-sym clause))
                  clauses)))))

(defmacro defpackage (name &rest clauses)
  (%%defpackage name clauses))

(defun string-input-stream (string)
  (vector 'string-input-stream 0 string))

(defun string-input-stream? (x)
  (and (vector? x)
       (eq? 'string-input-stream (vector-get x 0))))

(defparameter *standard-input* :stdin)

(defun read (&optional stream eof-thunk)
  (let* ((it (or stream *standard-input*))
         (result
           (cond
             ((eq? it :stdin)
              (read-from-stdin))
             ((string-input-stream? it)
              (read-from-string-stream! it))
             ((eq? 'function (type-of it))
              (call it))
             (t
              (error 'type-mismatch 'input-stream it)))))
    (if result (head result)
      (call eof-thunk))))
