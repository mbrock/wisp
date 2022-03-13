(progn (defun foo (x) (if (eq x 500000) x (foo (+ x 1)))) (foo 0))
