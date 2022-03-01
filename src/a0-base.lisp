(progn
  (set-function
   'LAMBDA
   (%macro-lambda (args body)
            (list 'lambda args (list 'progn body))))

  (set-function
   'DEFUN
   (%macro-lambda (name args body)
                  (list 'set-function (list 'quote name)
                        (list '%lambda args (list 'progn body)))))

  (set-function
   'DEFMACRO
   (%macro-lambda (name args body)
                  (list 'set-function (list 'quote name)
                        (list '%macro-lambda args (list 'progn body))))))
