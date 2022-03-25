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
