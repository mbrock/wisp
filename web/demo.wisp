(+ 20 22)

(map (fn (n) (* n n)) '(1 2 3 4))

(let ((name "world"))
  (string-append "Hello, " name "!"))

(do
  (defun square (n) (* n n))
  (square 9))
