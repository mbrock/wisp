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



;;; * Various useful functions and macros

(defmacro fn (params &rest body)
  `(%fn nil ,params ,(prognify body)))

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

(defun for-each (xs f)
  (if (nil? xs) nil
      (do
        (call f (head xs))
        (for-each (tail xs) f))))

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

(defmacro when (test &rest body)
  `(if ,test ,(prognify body) nil))

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

(defun send! (tag value)
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

(defun macroexpand (form)
  (iterative-fixpoint #'macroexpand-1 form))

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
                 (body (head (last form))))
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
                     (macroexpand-completely body)))
               (if (and (eq? bindings bindings-expansion)
                        (eq? body body-expansion))
                   form
                   (list 'let bindings-expansion body-expansion)))))
          ((eq? head 'quote) form)
          ((eq? head 'fn)
           (let ((params (second form))
                 (body (head (last form))))
             (let ((body-expansion
                     (macroexpand-completely body)))
               (if (eq? body body-expansion)
                   form
                   `(fn ,params ,body-expansion)))))
          (t
           (let ((expansion (macroexpand form)))
             (if (eq? form expansion)
                 form
                 (macroexpand-completely expansion))))))))

;;; Now we redefine DEFUN to use macroexpansion.
(defmacro defun (name args &rest body)
;  (print (list 'defun name args))
  (let ((expanded-body
          (macroexpand-completely
           (prognify body))))
    `(set-symbol-function! ',name (%fn ,name ,args ,expanded-body))))

(defmacro defun-noexpand (name args &rest body)
;  (print (list 'defun name args))
  `(set-symbol-function! ',name (%fn ,name ,args ,(prognify body))))

(defmacro defvar (var val)
  `(do
;     (print (list 'defvar ',var))
     (set-symbol-value! ',var ,val)))

;;; We can also mutate the code of a function or macro.
(defun compile! (function)
  (set-code! function (macroexpand-completely (code function))))

;;; Now we can go back and compile everything in the package.
(defun compile-many! (package)
  (for-each (reverse (package-symbols package))
    (fn (symbol)
      (let ((function (symbol-function symbol)))
        (when (and function
                   (not (jet? function)))
;          (print (list 'macroexpanding symbol))
          (compile! function))))))

(defun %split-string (string separator acc)
  (let* ((idx (string-search string separator)))
    (if idx
        (%split-string
         (string-slice string
                        (+ idx (string-length separator))
                        (string-length string))
         separator
         (cons (string-slice string 0 idx) acc))
        (reverse (cons string acc)))))

(defun split-string (string separator)
  (%split-string string separator nil))

(defun vector-each (vector function)
  (vector-each-loop vector function 0))

(defun vector-each-loop (vector function i)
  (when (< i (vector-length vector))
    (call function (vector-get vector i))
    (vector-each-loop vector function (+ i 1))))

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

(defun %filter (list predicate acc)
  (if (nil? list)
      (reverse acc)
    (let* ((x (head list))
           (next-acc (if (call predicate x)
                         (cons x acc)
                       acc)))
      (%filter (tail list) predicate next-acc))))

(defun filter (list predicate)
  (%filter list predicate nil))

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
