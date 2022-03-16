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

;;;
;;; NOTE: We can't use backquote in this file.
;;;

(set-symbol-function
 'DEFUN
 (%macro-lambda
  (name args &rest body)
  (list 'set-symbol-function (list 'quote name)
        (list 'lambda args (prognify body)))))

(set-symbol-function
 'DEFMACRO
 (%macro-lambda
  (name args &rest body)
  (list 'set-symbol-function (list 'quote name)
        (list '%macro-lambda args (prognify body)))))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))

(defmacro defvar (var val)
  (list 'set-symbol-value (list 'quote var) val))

(defmacro setq (var val)
  var  ;; evaluate it to trigger error if undefined
  (list 'set-symbol-value (list 'quote var) val))

(defun not (x)
  (if x nil t))

(defun atom (x)
  (not (eq 'cons (type-of x))))

(defun equal (x y)
  (if (eq x y) t
      (let ((xt (type-of x))
            (yt (type-of y)))
        (if (not (eq xt yt))
            nil
            (if (eq xt 'cons)
                (equal-lists x y)
                (error xt))))))

(defun equal-lists (x y)
  (if (eq x nil)
      (eq y nil)
      (if (eq y nil)
          nil
          (if (equal (car x) (car y))
              (equal-lists (cdr x) (cdr y))
              nil))))

(defmacro assert (x)
  (list 'if x nil (list 'error (list 'quote x))))

(defun null (x)
  (eq x nil))

(defun mapcar (f xs)
  (if (null xs) nil
      (cons (funcall f (car xs))
            (mapcar f (cdr xs)))))

(defun last (xs)
  (if (null xs) nil
      (if (null (cdr xs)) xs
          (last (cdr xs)))))

(defun %cond (clauses)
  (progn
    (if (null clauses) nil
        (let ((x (car clauses)))
          (list 'if
                (car x)
                (car (cdr x))
                (%cond (cdr clauses)))))))

(defmacro cond (&rest clauses)
  (%cond clauses))

(defun reduce-loop (f x xs)
  (cond ((null xs)
         x)
        ((null (cdr xs))
         (funcall f x (car xs)))
        (t
         (reduce-loop f
                      (funcall f x (car xs))
                      (cdr xs)))))

(defun reduce (f xs init)
  (reduce-loop f init xs))

(defun append-2 (xs ys)
  (if (null xs) ys
      (cons (car xs)
            (append (cdr xs) ys))))

(defun append (&rest xss)
  (reduce #'append-2 xss '()))

(defun remove-if (f xs)
  (if (null xs) nil
      (if (funcall f (car xs))
          (remove-if f (cdr xs))
          (cons (car xs) (remove-if f (cdr xs))))))

(defun some (f xs)
  (if (null xs) nil
      (if (funcall f (car xs))
          t
          (some f (cdr xs)))))

(defun butlast (xs)
  (if (null xs) nil
      (if (null (cdr xs)) nil
          (cons (car xs)
                (butlast (cdr xs))))))

(defun snoc (x y)
  (cons y x))

(defun reverse (list)
  (reduce #'snoc list nil))

(defun revappend (list tail)
  (reduce #'snoc list tail))

(defmacro and (x y)
  (list 'if x y nil))
