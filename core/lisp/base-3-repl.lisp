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
        (let ((exp (try (read src)
                     (catch (e k)
                       (returning nil
                         (print (list 'read-error e)))))))
          (cond
            ((eq? exp 'quit) 'bye)
            (t
             (progn
               (try (print (eval exp))
                 (catch (e k)
                   (print (list 'error e))
                   (print (list 'context (show-ktx k)))
                   (call k (ask))))
               (gc)
               (repl))))))))

(defun show-ktx (k)
  (ktx-show k '⛳))

(defun do-step! (run)
  (let ((now (run-exp run)))
    (progn
      (print (list 'context (ktx-show (run-way run)
                                      (list '⛳ (head now) (tail now)))))
      (step! run))))

(compile-many! (find-package "WISP"))



;;; * Asynchronous event loop using delimited continuation control

(defun make-actor (continuation)
  (let ((pid (fresh-symbol!))
        (inbox '()))
    (vector :actor pid inbox continuation)))

(defun actor-pid (actor)
  (vector-get actor 1))

(defun actor-inbox (actor)
  (vector-get actor 2))

(defun actor-continuation (actor)
  (vector-get actor 3))

(defun actor-push! (actor message)
  (vector-set! actor 2 (append (actor-inbox actor)
                               (list message))))

(defun set-actor-continuation! (actor continuation)
  (vector-set! actor 3 continuation))

(defun find-actor (actors pid)
  (if (nil? actors actors)
      (error 'no-such-actor pid)
      (let ((actor (head actors)))
        (if (eq? (actor-pid actor) pid)
            actor
            (find-actor (tail actors) pid)))))

(defun engine-act (actors self value)
  (let* ((yield-handler
           (fn (request continuation)
             (if (atom? request)
                 (error 'bad-request)
                 (ecase (head request)
                   (:spawn
                    (let ((actor (make-actor (second request))))
                      (progn
                        (set! actors (cons actor actors))
                        (set-actor-continuation! self continuation)
                        (engine-act actors self (actor-pid actor)))))
                   (:send
                    (let* ((pid (second request))
                           (message (third request))
                           (actor (find-actor actors pid)))
                      (actor-push! actor (cons (actor-pid self) message))
                      (set-actor-continuation! self continuation)
                      (engine-act actors self nil))))))))
    (returning actors
      (call-with-prompt :yield
          (fn () (call (actor-continuation self) value))
        yield-handler))))

(defun start-engine (root)
  (let* ((self (make-actor root))
         (actors (list self)))
    (engine-act actors self (actor-pid self))))

(defun spawn (function)
  (send! :yield (list :spawn function)))

(defun engine-example ()
  (start-engine (fn (self)
                  (let* ((a (spawn (fn (a)
                                     (progn
                                       (print (list 'a a))))))
                         (b (spawn (fn (b)
                                     (progn
                                       (print (list 'b b)))))))
                    (print (list 'a a 'b b))))))

(defun show-actors (actors)
  (map (fn (actor)
         (list (actor-pid actor)
               (actor-inbox actor)
               (show-ktx (actor-continuation actor))))
       actors))

(defun foo ()
  (show-actors (engine-example)))
