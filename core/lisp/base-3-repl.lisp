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
        (let ((me (cond
                    ((eq? fun 'if)
                     (list 'if terminus (head arg) (tail arg)))
                    ((eq? fun 'let)
                     (let ((current-symbol (head acc))
                           (let-acc (parse-let-acc (tail acc) nil)))
                       (list 'let
                             (append (head let-acc)
                                     (cons (list current-symbol
                                                 terminus)
                                           arg))
                             (tail let-acc))))
                    ((eq? fun 'prompt)
                     (list 'prompt acc terminus (list arg)))
                    (t
                     (append (list fun)
                             acc
                             (list terminus)
                             arg)))))
          (ktx-show hop me)))))

(defun ask ()
  (progn
    (write "use this instead> ")
    (read (read-line))))

(defun repl ()
  (write "> ")
  (let ((src (read-line)))
    (if (nil? src)
        'bye
        (let ((exp (read src)))
          (progn
            (handle
             'error
             (fn () (print (eval exp)))
             (fn (condition continuation)
                 (progn
                   (print (list 'error condition))
                   (print
                    (list 'context
                          (show-ktx continuation)))
                   (call continuation (ask)))))
            (repl))))))

(defun show-ktx (k)
  (ktx-show k '⛳))

(defun foo-test ()
  (handle 'foo (fn () (send! 'foo 'bar)) (fn (v k) v)))

(defun do-step! (run)
  (let ((now (run-exp run)))
    (progn
      (print (list 'context (ktx-show (run-way run)
                                      (list '⛳ (head now) (tail now)))))
      (step! run))))
