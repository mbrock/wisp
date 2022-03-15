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

(defvar *bq-quote* '#:bq-quote)
(defvar *bq-list* '#:bq-list)
(defvar *bq-append* '#:bq-append)

(defun bq-completely-process (x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens raw-result)))

(defun bq-loop (p q)
  (progn
    (print (list 'bq-loop p q))
    (if (atom p)
        (cons *bq-append*
              (revappend q (list (list *bq-quote* p))))
        (cond ((eq (car p) 'unquote)
               (if (null (cddr p))
                   (cons *bq-append*
                         (revappend q (list (cadr p))))
                   (error "malformed ,")))
              ((eq (car p) 'unquote-splicing)
               (error "dotted ,@"))
              (t
               (bq-loop (cdr p)
                        (cons (bracket (car p)) q)))))))

(defun bq-process (x)
  (progn
    (print (list 'processing x))
    (cond ((atom x)
           (list *bq-quote* x))
          ((eq (car x) 'backquote)
           (bq-process (bq-completely-process (cadr x))))
          ((eq (car x) 'unquote)
           (cadr x))
          ((eq (car x) 'unquote-splicing)
           (error ",@ after `"))
          (t (bq-loop x nil)))))

(defun bracket (x)
  (cond ((atom x)
         (list *bq-list* (bq-process x)))
        ((eq (car q) 'unquote)
         (list *bq-list* (cadr x)))
        ((eq (car x) 'unquote-splicing)
         (cadr x))
        (t (list *bq-list* (bq-process x)))))

(defun eql (a b)
  (prog1 (eq a b)
    (print (list 'eql (eq a b) a b))))

(defun maptree (fn x)
  (progn
    (print (list 'in-maptree (env)))
    (if (atom x)
        (funcall fn x)
        (let ((a (funcall fn (car x)))
              (d (maptree fn (cdr x))))
          (if (and (eq a (car x)) (eq d (cdr x)))
              x
              (cons a d))))))

(defun bq-remove-tokens (x)
  (progn
    (print (list 'bq-remove-tokens x))
    (cond ((eql x *bq-list*) 'list)
          ((eql x *bq-append*) 'append)
          ((eql x *bq-quote*) 'quote)
          ((atom x) x)
          (t (maptree #'bq-remove-tokens x)))))

(defmacro quasiquote (x)
  (bq-completely-process x))

(defmacro foobar (x)
  x)
