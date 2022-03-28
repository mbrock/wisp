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

(defmacro set! (var val)
  (list '%set! (list 'quote var) val))

(defun not (x)
  (if x nil t))

(defmacro and-2 (x y)
  (list 'if x y x))

(defmacro or-2 (x y)
  (list 'if x x y))

(defun atom? (x)
  (not (eq? 'cons (type-of x))))

(defun pair? (x)
  (eq? 'cons (type-of x)))

(defun list? (x)
  (or (pair? x) (nil? x)))

(defun equal? (x y)
  (if (eq? x y) t
    (let ((xt (type-of x))
          (yt (type-of y)))
      (if (not (eq? xt yt))
          nil
        (if (eq? xt 'cons)
            (equal-lists? x y)
          (error xt))))))

(defun equal-lists? (x y)
  (if (eq? x nil)
      (eq? y nil)
      (if (eq? y nil)
          nil
          (if (equal? (head x) (head y))
              (equal-lists? (tail x) (tail y))
              nil))))

(defun nil? (x)
  (eq? x nil))

(defun map (f xs)
  (if (nil? xs) nil
      (cons (call f (head xs))
            (map f (tail xs)))))

(defun last (xs)
  (if (nil? xs) nil
      (if (nil? (tail xs)) xs
          (last (tail xs)))))

(defun second (xs)
  (head (tail xs)))

(defun third (xs)
  (head (tail (tail xs))))

(defun %cond (clauses)
  (if (nil? clauses) nil
      (let ((x (head clauses)))
        (list 'if
              (head x)
              (head (tail x))
              (%cond (tail clauses))))))

(defmacro cond (&rest clauses)
  (%cond clauses))

(defun reduce-loop (f x xs)
  (cond ((nil? xs) x)
        ((nil? (tail xs))
         (call f x (head xs)))
        (t
         (reduce-loop f
                      (call f x (head xs))
                      (tail xs)))))

(defun length-aux (xs n)
  (if (nil? xs) n
      (length-aux (tail xs) (+ n 1))))

(defun length (xs)
  (length-aux xs 0))

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

(defun snoc (x y)
  (cons y x))

(defun reverse (list)
  (reduce #'snoc list nil))

(defun reverse-append (list tail)
  (reduce #'snoc list tail))
