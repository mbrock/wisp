;; -*- fill-column: 64; -*-
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

;; Needed to run this in Wisp:
;;
;;  - rest parameters
;;  - keyword parameters
;;
;;  - DEFSTRUCT
;;  - LET*
;;
;;  - COND
;;  - DOLIST
;;
;;  - ASSOC
;;  - LENGTH
;;  - SORT
;;  - SUBSEQ
;;
;;  - <=
;;  - ATOM
;;  - LISTP
;;  - MAKE-STRING
;;  - MAX
;;  - PUSH
;;

(defpackage #:prty (:use #:common-lisp))
(in-package #:prty)

(defvar *max-width* 62)

(defun strcat (a b)
  (concatenate 'string a b))

(defun indent (n xs)
  (mapcar (lambda (x)
            (strcat (make-string n :initial-element #\ ) x)) xs))

(defstruct box
  len  ;; height of box minus one
  fin  ;; width of final line
  max  ;; max width of all lines
  txt  ;; list of lines
  )

(defun text-box (s)
  (make-box :len 0
            :fin (length s)
            :max (length s)
            :txt (list s)))

(defun indent-box (a n)
  (make-box :len (box-len a)
            :fin (+ n (box-fin a))
            :max (+ n (box-max a))
            :txt (indent n (box-txt a))))

(defun flush-box (a)
  (make-box :len (+ 1 (box-len a))
            :fin 0
            :max (box-max a)
            :txt (append (box-txt a) '(""))))

;; aaaaa   <> bbbbb
;; aa         bbbbbbb
;;
;; = aaaaa      all but last a
;;   aabbbbb    last a ++ first b
;;     bbbbbbb  all but first b, indented
(defun box-hcat (a b)
  (make-box :len (+ (box-len a) (box-len b))
            :fin (+ (box-fin a) (box-fin b))
            :max (max (box-max a) (+ (box-max b)
                                     (box-fin a)))
            :txt (let* ((as (box-txt a))
                        (bs (box-txt b))
                        (last-a (car (last as))))
                   (append (butlast as)
                           (cons (strcat last-a (car bs))
                                 (indent (length last-a)
                                         (cdr bs)))))))

(defun box-vcat (a b)
  (box-hcat (flush-box a) b))

(defun box<= (a b)
  (and (<= (box-len a) (box-len b))
       (<= (box-max a) (box-max b))
       (<= (box-fin a) (box-fin b))))

(defun box-render (a)
  (format nil "~{~A~^~%~}" (box-txt a)))

(defun pareto-loop (acc list)
  (if (null list) acc
      (let* ((x (car list))
             (xs (cdr list))
             (better-than-x (lambda (y)
                              (box<= y x)))
             (worse-than-x (lambda (y)
                             (box<= x y))))
        (if (some better-than-x acc)
            (pareto-loop acc xs)
            (pareto-loop
             (cons x (remove-if worse-than-x acc)) xs)))))

(defun pareto (xs)
  (pareto-loop nil xs))

(defun box-valid (a)
  (< (box-max a) *max-width*))

(defun hcat (xs ys)
  (let ((candidates nil))
    (dolist (x xs)
      (dolist (y ys)
        (let ((xy (box-hcat x y)))
          (when (box-valid xy)
            (push xy candidates)))))
    (pareto candidates)))

(defun choose (xs ys)
  (pareto (append xs ys)))

(defun flush (xs)
  (pareto (mapcar #'flush-box xs)))

(defun vcat (a b)
  (hcat (flush a) b))

(defun text (s)
  (remove-if-not #'box-valid (list (text-box s))))

(defun cat (xs ys)
  (choose (hcat xs ys) (vcat xs ys)))

(defun render (xs)
  (box-render
   (car (sort xs (lambda (a b)
                   (< (box-len a)
                      (box-len b)))))))

(render
 (cat (text "aaaaaa")
      (text "bbbbbb")))

(mapcar #'box-valid (text "aaaaaaaa"))

(defun hjoin (xs)
  (reduce #'hcat xs))

(defun hspace (a b)
  (hcat a (hcat (text " ") b)))

(defun hsep (xs)
  (reduce #'hspace xs))

(defun vjoin (xs)
  (reduce #'vcat xs))

(defun shove (a)
  (mapcar (lambda (x) (indent-box x 1)) a))

(defun join (xs)
  (if (null xs)
      (text "")
      (choose (hsep xs) (vjoin xs))))

(defparameter *hang* '((defun . 2)
                       (cond . 0)
                       (if . 1)
                       (let . 1)
                       ))

(defmacro defun-save-code (f args body)
  (list 'progn
        (list 'defun f args body)
        (list 'setf
              (list 'get (list 'quote f) (list 'quote 'code))
              (list 'quote (list 'defun f args body)))))

(defun-save-code pretty (x)
  (cond
    ((atom x)
     (text (format nil "~s" x)))
    ((listp x)
     (let ((hang (assoc (car x) *hang*)))
       (if hang
           (let ((n (+ 1 (cdr hang))))
             (hjoin
              (list (text "(")
                    (vcat (join (mapcar #'pretty (subseq x 0 n)))
                          (shove
                           (hcat (join
                                  (mapcar #'pretty (subseq x n)))
                                 (text ")")))))))
           (if (> (length x) 2)
               (hjoin (list (text "(")
                            (pretty (car x))
                            (text " ")
                            (join (mapcar #'pretty (cdr x)))
                            (text ")")))
               (hjoin (list (text "(")
                            (join (mapcar #'pretty x))
                            (text ")")))
               )
           )))))

(defun foo ()
  (format t "~a"
          (render
           (pretty
            (get 'pretty 'code)))))
