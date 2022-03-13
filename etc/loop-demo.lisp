(progn (defun foo (x) (if (eq x 5000000) x (foo (+ x 1)))) (foo 0))
