;;
;; This file is part of Wisp.
;;
;; Wisp is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; Wisp is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
;; Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with Wisp. If not, see
;; <https://www.gnu.org/licenses/>.
;;

(defpackage #:prty (:use #:common-lisp))

(defstruct layout
  height last-width max-width lines)

(defun indent (n s)
  (concatenate 'string (make-string n :initial-element #\ ) s))

(defun layout-hcat (a b)
  (make-layout
   :height (+ (layout-height a) (layout-height b))
   :last-width (+ (layout-last-width a) (layout-last-width b))
   :max-width (max (layout-max-width a)
                   (+ (layout-max-width b)
                      (layout-last-width a)))
   :lines
   (let* ((xs (layout-lines a))
          (ys (layout-lines b))
          (x (last xs))
          (n (length x)))
     (append (butlast xs) x (cdr ys)
             (mapcar (lambda (y) (indent n y)) (cdr ys))))))

(defun layout-vcat (a b)
  (layout-hcat (flush-layout a) b))

(defun text-layout (s)
  (make-layout
   :height 0
   :last-width (length s)
   :max-width (length s)
   :lines (list s)))

(defun flush-layout (a)
  (make-layout
   :height (+ 1 (layout-height a))
   :last-width 0
   :max-width (layout-max-width a)
   :lines (append (layout-lines a) '(""))))

(defun layout< (a b)
  (and (< (layout-height a) (layout-height b))
       (< (layout-max-width a) (layout-max-width b))
       (< (layout-last-width a) (layout-last-width b))))

(defun layout-render (a)
  (format nil "~{~A~^~%~}" (layout-lines a)))

(defun pareto (xs <)
  (pareto-loop < nil xs))

(defun pareto-loop (< acc list)
  (if (null list) acc
      (let ((x (car acc)) (xs (cdr acc)))
        (if (some (lambda (a) (funcall < a x)) acc)
            (pareto-loop < acc xs)
            (pareto-loop <
             (cons x (remove-if-not (lambda (a) (< x a)) acc))
             xs)))))

(defun layout-valid (a)
  (< (layout-max-width a) 40))

(defun <> (xs ys)
  (let ((candidates nil))
    (loop for x in xs
          do (loop for y in ys
                   do (let ((xy (layout-hcat x y)))
                        (when (layout-valid xy)
                          (push xy candidates)))))
    (pareto candidates #'layout<)))

(defun choose (xs ys)
  (pareto (append xs ys) #'layout<))

(defun flush (xs)
  (pareto (mapcar #'flush-layout xs) #'layout<))

(defun text (s)
  (remove-if-not #'layout-valid (list (text-layout s))))
