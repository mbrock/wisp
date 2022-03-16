(defvar *suspension* nil)

(defun fix (f)
  (funcall f f))

(defun engine (thread-a thread-b)
  (let ((handler
          (fix (lambda (self)
                 (lambda (request continuation)
                   (setq *suspension*
                         (cons request
                               (lambda (x)
                                 (handle 'await
                                         (lambda ()
                                           (funcall continuation x))
                                         (fix self))))))))))
    (handle 'await
            (lambda ()
              (progn
                (funcall thread-a)
                (funcall thread-b)
                (setq *suspension* nil)))
            handler))
  (list 'blocked-on (car *suspension*) 'until '(resume x)))

(defun resume (x)
  (if *suspension*
      (funcall (cdr *suspension*) x)
      'done))

(let ((a nil)
      (b nil))
  (progn
    (engine (lambda ()
              (setq a
                    (+ (send 'await '(gimme number))
                       (send 'await '(gimme another)))))
            (lambda ()
              (setq b
                    (* (send 'await '(one more))
                       (send 'await '(last one))))))

    (resume 1)
    (resume 2)
    (resume 3)
    (resume 4)
    (list 'done 'a a 'b b)))

(list 'ok
      (handle 'tag
              (lambda ()
                (* 2 (send 'tag 5)))
              (lambda (v k)
                (list '(continuation k)
                      '(result (funcall k (+ 1 v)))))))
