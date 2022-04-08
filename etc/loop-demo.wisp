(do (defun foo (x) (if (eq x 50000) x (foo (+ x 1)))) (foo 0))
