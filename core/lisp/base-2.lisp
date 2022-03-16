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

(defmacro with-trace (&rest body)
  `(progn
     (wtf t)
     (prog1 ,(prognify body)
       (wtf nil))))

(defun base-test ()
  (assert (equal 1 1))
  (assert (equal '(1 2 3) '(1 2 3)))
  (assert (equal '((1 2) (3 4)) '((1 2) (3 4))))
  (assert (not (equal '(1) '(1 2))))

  (defvar *x* 1)
  (assert (eq *x* 1))
  (setq *x* 2)
  (assert (eq *x* 2)))

(defun test-call/cc ()
  (defvar *plusser* nil)
  (call/cc (lambda (break)
             (progn
               (+ 10 (call/cc (lambda (k)
                                (progn
                                  (setq *plusser* k)
                                  (funcall break nil))))))))
  (assert (eq 11 (funcall *plusser* 1))))

(defmacro or (a b)
  `(if ,a ,b nil))

(defun test-1 ()
  (handle 'foo
          (lambda ()
            (+ 5 (send 'foo 'bar)))
          (lambda (v k)
            (+ 1 (funcall k 3)))))
