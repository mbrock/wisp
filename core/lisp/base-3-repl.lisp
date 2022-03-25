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

(defun ask (continuation)
  (write ";; Restarts:
;;   (1) Escape to REPL
;;   (2) Input another value
;;   (3) Quit
")
  (write "*> ")
  (let ((choice (read (read-line))))
    (ecase choice
      (1 nil)
      (2 (progn
           (write "value> ")
           (call continuation (eval (read (read-line))))))
      (3 (error 'quit)))))

(defun repl ()
  (write "> ")
  (let ((exp (try (read)
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
             (ask k)))
         (gc)
         (repl))))))

(defmacro with-simple-error-handler (dummy &rest body)
  `(try ,(prognify body)
     (catch (e k)
       (print (list 'error e))
       (print (list 'context (show-ktx k)))
       (unhandled-error e))))

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

(defun function-continuation (function)
  (call-with-prompt :prompt
      (fn ()
        (call function (send-with-default! :prompt nil nil)))
    (fn (x continuation)
      continuation)))

(defun make-actor (function)
  (let ((pid (fresh-symbol!)))
    (let ((inbox '())
          (continuation (function-continuation
                         (fn (self)
                          (let ((result (call function self)))
                            (send! :yield (list :exit result))))))
          (answer (list pid))
          (result nil)
          (state :started))
      (vector :actor pid inbox continuation answer result state))))

(defun actor-pid (actor)
  (vector-get actor 1))

(defun actor-inbox (actor) (vector-get actor 2))
(defun actor-continuation (actor) (vector-get actor 3))
(defun actor-answer (actor) (vector-get actor 4))
(defun actor-result (actor) (vector-get actor 5))
(defun actor-state (actor) (vector-get actor 6))

(defun set-actor-state! (actor state)
  (vector-set! actor 6 state))

(defun actor-push-message! (actor message)
  (set-actor-inbox! actor (append (actor-inbox actor)
                                  (list message))))

(defun actor-take-message! (actor)
  (let ((messages (actor-inbox actor)))
    (if (nil? messages)
        (error 'empty-inbox)
      (returning (head messages)
        (set-actor-inbox! actor (tail messages))))))

(defun set-actor-inbox! (actor inbox)
  (vector-set! actor 2 inbox))

(defun set-actor-continuation! (actor continuation)
  (vector-set! actor 3 continuation))

(defun set-actor-answer! (actor answer)
  (vector-set! actor 4 (list answer)))

(defun set-actor-result! (actor result)
  (vector-set! actor 5 (list result)))

(defun actor-clear-answer! (actor)
  (returning (head (actor-answer actor))
    (vector-set! actor 4 nil)))

(defun find-actor (actors pid)
  (if (nil? actors)
      (error 'no-such-actor pid)
    (let ((actor (head actors)))
      (if (eq? (actor-pid actor) pid)
          actor
        (find-actor (tail actors) pid)))))

(defun remove (list element)
  (if (nil? list) nil
    (let ((head (head list)))
      (if (eq? head element)
          (tail list)
        (cons head (remove (tail list) element))))))

(defun find (list predicate)
  (if (nil? list)
      nil
    (let ((x (head list)))
      (if (call predicate x)
          (cons x nil)
        (find (tail list) predicate)))))

(defun make-queue (items)
  (list items))

(defun queue-items (queue)
  (head queue))

(defun enqueue! (queue item)
  (set-head! queue (append (queue-items queue) (list item))))

(defun enqueue-first! (queue item)
  (set-head! queue (cons item (queue-items queue))))

(defun queue-remove! (queue item)
  (set-head! queue (remove (queue-items queue) item)))

(defun find-ready-actor (actors)
  (find (queue-items actors)
        (fn (actor)
          (or (actor-answer actor)
              (and (eq? :receiving (actor-state actor))
                   (actor-inbox actor))))))

(defun move-to-back-of-queue! (queue thing)
  (queue-remove! queue thing)
  (enqueue! queue thing))

(defun make-yield-handler (self actors)
  (fn (request continuation)
    (if (atom? request)
        (error 'invalid-yield-request request)
      (returning t
        (set-actor-continuation! self continuation)
        (ecase (head request)
          (:spawn
           (do-spawn actors self request continuation))
          (:send
           (do-send actors self request continuation))
          (:receive
           (do-receive actors self request continuation))
          (:exit
           (do-exit actors self request)))))))

(defun do-spawn (actors self request continuation)
  (let* ((child (make-actor (second request))))
    (enqueue-first! actors child)
    (set-actor-answer! self (actor-pid child))))

(defun do-send (actors self request continuation)
  (let* ((pid (second request))
         (recipient (find-actor (queue-items actors) pid))
         (message (third request)))
    (set-actor-answer! self t)
    (actor-push-message! recipient message)
    (when (eq? :receiving (actor-state recipient))
      (enqueue-first! actors recipient))))

(defun do-receive (actors self request continuation)
  (if (actor-inbox self)
      (let ((message (actor-take-message! self)))
        (set-actor-answer! self message)
        (set-actor-state! self :running)
        (enqueue-first! actors self))
    (progn
      (set-actor-state! self :receiving))))

(defun do-exit (actors self request)
  (let ((result (second request)))
    (progn
      (set-actor-result! self result)
      (set-actor-state! self :done)
      (set-actor-continuation! self nil))))

(defmacro when-result (var expr &rest body)
  `(let ((,var ,expr))
     (unless (nil? ,var)
       (set! ,var (head ,var))
       ,@body)))

(defun deliver-messages (actors)
  (for-each (queue-items actors)
    (fn (actor)
      (when (and (eq? :receiving (actor-state actor))
                 (actor-inbox actor))
        (let ((message (actor-take-message! actor)))
          (progn
            (set-actor-answer! actor message)
            (set-actor-state! actor :ready)))))))

(with-simple-error-handler ()
  ;; If any actor has an answer, this function moves that actor
  ;; to the back of the queue and executes its continuation until
  ;; it yields again.
  (defun engine-act (actors)
    (deliver-messages actors)
    (when-result self (find-ready-actor actors)
      (move-to-back-of-queue! actors self)
      (call-with-prompt :yield
          (fn () (call (actor-continuation self)
                       (actor-clear-answer! self)))
        (make-yield-handler self actors)))))

(defun make-engine (root)
  (let* ((self (make-actor root))
         (actors (make-queue (list self))))
    actors))

(defun spawn (function)
  (send! :yield (list :spawn function)))

(defun send-message (pid message)
  (send! :yield (list :send pid message)))

(defun receive-message ()
  (send! :yield (list :receive)))

(defun show-actors (actors)
  (map (fn (actor)
         (list :pid (actor-pid actor)
               :inbox (actor-inbox actor)
               ;;               (show-ktx (actor-continuation actor))
               :answer (actor-answer actor)))
       (queue-items actors)))

(defun %loop (thunk)
  (call thunk)
  (%loop thunk))

(defun loop (thunk)
  (handle (%loop thunk)
    (:break (value k) value)))

(defun break (value)
  (send! :break value))

(defun engine-example ()
  (make-engine
   (fn (self)
     (let* ((a (spawn (fn (a)
                        (returning 'a-done
                          (print (list 'a :started a))
                          (let* ((b (receive-message)))
                            (send-message b '(hello b from a))
                            (print (list 'a :receive 1 (receive-message)))
                            (print (list 'a :receive 2 (receive-message)))
                            (print (list 'a :receive 3 (receive-message))))
                          ))))
            (b (spawn (fn (b)
                        (returning 'b-done
                          (print (list 'b :started b))
                          (send-message a b)
                          (print (list 'b :receive 1 (receive-message)))
                          (print (list 'b :receive 2 (receive-message)))
                          (print (list 'b :receive 3 (receive-message))))))))
       (returning 'root-done
         (print (list :spawned a b))
         (for-each '(1 2 3)
           (fn (i) (send-message a (list 'hello 'a i))))
         (for-each '(1 2 3)
           (fn (i) (send-message b (list 'hello 'b i)))))))))

(defun exhaust-engine (engine)
  (returning nil
    (loop (fn ()
            (unless (engine-act engine)
              (break engine))))))

(defun step-engine-example ()
  (let ((engine (engine-example)))
    (loop (fn ()
            (print (show-actors engine))
            (write "engine> ")
            (read-line)
            (unless (engine-act engine)
              (break engine))))))

(defun run-engine-example ()
  (exhaust-engine (engine-example)))
