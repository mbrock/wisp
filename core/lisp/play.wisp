(defvar *suspension* nil)

(defun fix (f)
  (call f f))

(defun engine (thread-a thread-b)
  (let ((handler
          (fix (fn (self)
                 (fn (request continuation)
                   (set! *suspension*
                         (cons request
                               (fn (x)
                                 (handle 'await
                                         (fn () (call continuation x))
                                         (fix self))))))))))
    (handle 'await
            (fn ()
              (progn
                (call thread-a)
                (call thread-b)
                (set! *suspension* nil)))
            handler))
  (list 'blocked-on (head *suspension*) 'until '(resume x)))

(defun resume! (x)
  (print x)
  (if *suspension*
      (call (tail *suspension*) x)
      'done))

(let ((a nil)
      (b nil))
  (progn
    (engine (fn ()
                (set! a
                      (+ (send! 'await '(gimme number))
                         (send! 'await '(gimme another)))))
            (fn ()
                (set! b
                      (* (send! 'await '(one more))
                         (send! 'await '(last one))))))
    (with-trace
        (progn
          (resume! 1)
          (resume! 2)
          (resume! 3)
          (resume! 4)
          (list 'done 'a a 'b b)))))
