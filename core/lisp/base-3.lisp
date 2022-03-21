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

(defun parse-let-acc (acc bindings)
  (if (nil? (tail acc))
      (cons bindings (head acc))
      (let ((expr (head acc))
            (name (head (tail acc)))
            (rest (tail (tail acc))))
        (parse-let-acc rest (cons (list name expr)
                                  bindings)))))

(defun ktx-show (ktx terminus)
  (if (top? ktx) terminus
      (let ((fun (ktx-fun ktx))
            (acc (ktx-acc ktx))
            (arg (ktx-arg ktx))
            (hop (ktx-hop ktx)))
        (cond
          ((eq? fun 'if)
           (list 'if (ktx-show hop terminus) (head arg) (tail arg)))
          ((eq? fun 'let)
           (let ((current-symbol (head acc))
                 (let-acc (parse-let-acc (tail acc) nil)))
             (list 'let
                   (append (head let-acc)
                           (cons (list current-symbol
                                       (ktx-show hop terminus))
                                 arg))
                   (tail let-acc))))
          (t
           (append (list fun)
                   acc
                   (list (ktx-show hop terminus))
                   arg))))))

(defun repl ()
  (write "> ")
  (let ((src (read-line)))
    (let ((exp (read src)))
      (let ((val (eval exp)))
        (progn
          (print val)
          (repl))))))
