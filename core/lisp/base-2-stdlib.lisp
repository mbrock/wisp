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

(defmacro assert (x)
  `(if ,x nil (error ',x)))

(defmacro with-trace (&rest body)
  `(progn
     (wtf t)
     (returning ,(prognify body)
       (wtf nil))))

(defmacro handle (body clause)
  (let ((tag-name (head clause))
        (handler-args (head (tail clause)))
        (handler-body (tail (tail clause))))
    `(call-with-prompt ',tag-name
       (fn () ,body)
       (fn ,handler-args ,(prognify handler-body)))))

(defmacro try (body clause)
  (let ((catch (head clause))
        (handler-args (head (tail clause)))
        (handler-body (tail (tail clause))))
    `(call-with-prompt 'error
       (fn () ,body)
       (fn ,handler-args ,(prognify handler-body)))))

(defun fixpoint (f x)
  (let ((y (call f x)))
    (if (eq? x y) x
        (fixpoint f y))))

(defun macroexpand (form)
  (fixpoint #'macroexpand-1 form))

(defun macroexpand-completely (form)
  (fixpoint #'macroexpand-recursively form))

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

(defmacro defun (name args &rest body)
  (progn
    (print (list 'defun name args))
    (let ((expanded-body
            (macroexpand-completely
             (prognify body))))
      `(set-symbol-function! ',name (fn ,args ,expanded-body)))))

(defun compile! (function)
  (set-code! function (macroexpand-completely (code function))))

(defun each! (xs f)
  (if (nil? xs) nil
      (progn
        (call f (head xs))
        (each! (tail xs) f))))

(defmacro when (condition &rest body)
  `(if ,condition ,(prognify body) nil))

(defun compile-many! (package)
  (each! (package-symbols package)
         (fn (symbol)
           (let ((function (symbol-function symbol)))
             (when (and function
                        (not (jet? function)))
               (print (list 'macroexpanding symbol))
               (compile! function))))))
