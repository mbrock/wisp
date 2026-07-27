(import (scheme base)
        (scheme cxr)
        (scheme process-context)
        (scheme time)
        (scheme write))

(define tak-input '(18 12 6))
(define deriv-input
  '(+ (* 3 x x) (* a x x) (* b x) 5))
(define deriv-expected
  '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
      (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
      (* (* b x) (+ (/ 0 b) (/ 1 x)))
      0))

(define (make-nil-list count)
  (let loop ((count count) (result '()))
    (if (= count 0)
        result
        (loop (- count 1) (cons '() result)))))

(define dividend (make-nil-list 1000))
(define stdlib-input
  (let loop ((value 63) (result '()))
    (if (< value 0)
        result
        (loop (- value 1) (cons value result)))))
(define backquote-input
  '(a (b (unquote c)) (unquote-splicing d) e))
(define backquote-expected
  '(append
    (list (quote a))
    (list (append (list (quote b)) (list c) (quote ())))
    d
    (list (quote e))
    (quote ())))

(define bq-quote (string->symbol "private-bq-quote"))
(define bq-list (string->symbol "private-bq-list"))
(define bq-append (string->symbol "private-bq-append"))

(define (atom? value) (not (pair? value)))

(define (reverse-append values tail)
  (let loop ((values values) (result tail))
    (if (null? values)
        result
        (loop (cdr values) (cons (car values) result)))))

(define (bq-completely-process value)
  (bq-remove-tokens (bq-process value)))

(define (bq-loop p q)
  (if (atom? p)
      (cons bq-append
            (reverse-append q (list (list bq-quote p))))
      (cond
        ((eq? (car p) 'unquote)
         (if (null? (cddr p))
             (cons bq-append
                   (reverse-append q (list (cadr p))))
             (error "malformed unquote")))
        ((eq? (car p) 'unquote-splicing)
         (error "dotted unquote-splicing"))
        (else (bq-loop (cdr p) (cons (bracket (car p)) q))))))

(define (bq-process value)
  (cond
    ((atom? value) (list bq-quote value))
    ((eq? (car value) 'backquote)
     (bq-process (bq-completely-process (cadr value))))
    ((eq? (car value) 'unquote) (cadr value))
    ((eq? (car value) 'unquote-splicing)
     (error "unquote-splicing after backquote"))
    (else (bq-loop value '()))))

(define (bracket value)
  (cond
    ((atom? value) (list bq-list (bq-process value)))
    ((eq? (car value) 'unquote)
     (list bq-list (cadr value)))
    ((eq? (car value) 'unquote-splicing) (cadr value))
    (else (list bq-list (bq-process value)))))

(define (bq-remove-tokens value)
  (cond
    ((eq? value bq-list) 'list)
    ((eq? value bq-append) 'append)
    ((eq? value bq-quote) 'quote)
    ((atom? value) value)
    (else
     (cons (bq-remove-tokens (car value))
           (bq-remove-tokens (cdr value))))))

(define (stdlib-list)
  (let* ((mapped (map (lambda (value) (+ value 1))
                      stdlib-input))
         (mirrored (append mapped (reverse mapped)))
         (large
          (let loop ((values mirrored) (result '()))
            (cond
              ((null? values) (reverse result))
              ((< (car values) 32)
               (loop (cdr values) result))
              (else
               (loop (cdr values)
                     (cons (car values) result)))))))
    (length large)))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (deriv value)
  (cond
    ((not (pair? value)) (if (eq? value 'x) 1 0))
    ((or (eq? (car value) '+) (eq? (car value) '-))
     (cons (car value) (map deriv (cdr value))))
    ((eq? (car value) '*)
     (list '* value
           (cons '+
                 (map (lambda (argument)
                        (list '/ (deriv argument) argument))
                      (cdr value)))))
    ((eq? (car value) '/)
     (list '-
           (list '/ (deriv (cadr value)) (caddr value))
           (list '/ (cadr value)
                 (list '* (caddr value) (caddr value)
                       (deriv (caddr value))))))
    (else (error "no derivation method available"))))

(define (diviter value)
  (let loop ((value value) (result '()))
    (if (null? value)
        result
        (loop (cddr value) (cons (car value) result)))))

(define (divrec value)
  (if (null? value)
      '()
      (cons (car value) (divrec (cddr value)))))

(define (run-once name)
  (cond
    ((string=? name "tak") (apply tak tak-input))
    ((string=? name "deriv") (deriv deriv-input))
    ((string=? name "diviter") (diviter dividend))
    ((string=? name "divrec") (divrec dividend))
    ((string=? name "stdlib-list") (stdlib-list))
    ((string=? name "backquote")
     (bq-completely-process backquote-input))
    (else (error "unknown benchmark" name))))

(define (correct? name result)
  (cond
    ((string=? name "tak") (= result 7))
    ((string=? name "deriv") (equal? result deriv-expected))
    ((or (string=? name "diviter") (string=? name "divrec"))
     (= (length result) 500))
    ((string=? name "stdlib-list") (= result 66))
    ((string=? name "backquote")
     (equal? result backquote-expected))
    (else #f)))

(define arguments (cdr (command-line)))
(define name (car arguments))
(define iterations (string->number (cadr arguments)))
(define warmup
  (if (pair? (cddr arguments))
      (string->number (caddr arguments))
      0))

(when (< iterations 1) (error "iterations must be positive"))

(let loop ((remaining warmup))
  (unless (= remaining 0)
    (run-once name)
    (loop (- remaining 1))))

(define started (current-jiffy))
(define result
  (let loop ((remaining iterations) (result #f))
    (if (= remaining 0)
        result
        (loop (- remaining 1) (run-once name)))))
(define elapsed-ns
  (quotient
   (* (- (current-jiffy) started) 1000000000)
   (jiffies-per-second)))

(unless (correct? name result) (error "wrong result" name))

(display "{\"benchmark\":\"")
(display name)
(display "\",\"iterations\":")
(display iterations)
(display ",\"elapsed_ns\":")
(display elapsed-ns)
(display ",\"ns_per_iteration\":")
(display (quotient elapsed-ns iterations))
(display "}")
(newline)
