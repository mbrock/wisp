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
       (progn ,@body ,x-var))))

(defmacro with-trace (&rest body)
  `(progn
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
      (progn
        (call f (head xs))
        (for-each (tail xs) f))))

(defun vector-for-each (xs f)
  (%vector-for-each xs f 0))

(defun %vector-for-each (xs f i)
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



;;; * Parameters with dynamic scope
;;;
;;; A dynamic binding is just a prompt with a handler that
;;; resumes its continuation with the binding's value.
;;;
;;; Each dynamic variable ("parameter") has its own prompt tag.
;;; The prompt tag is the value returned by ~MAKE-PARAMETER~.
;;;
;;; We also allow "mutating" the current dynamic binding of a
;;; parameter by sending a singleton list to its prompt.
;;;
;;; If we had something like ~DEFINE-SYMBOL-MACRO~, we could
;;; access parameters as if they were variables... that would
;;; be nice.

(defun make-parameter (name default-value)
  (list name default-value))

(defun parameter (parameter)
  (let ((default (second parameter)))
    (send-with-default! parameter 'get default)))

(defun call-with-parameter (parameter value function)
  (call-with-prompt parameter function
    (fn (request continuation)
      (let ((next-value (if (eq? request 'get)
                            value
                            (head request))))
        (call-with-parameter parameter next-value
          (fn () (call continuation next-value)))))))

(defmacro with (parameter value &rest body)
  `(call-with-parameter ,parameter ,value
     (fn () ,@body)))



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
               (eq? head 'progn))
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
  `(progn
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
