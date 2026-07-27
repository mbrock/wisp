#lang racket/base

(require json
         racket/list)

(define tak-input '(18 12 6))
(define deriv-input
  '(+ (* 3 x x) (* a x x) (* b x) 5))
(define deriv-expected
  '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
      (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
      (* (* b x) (+ (/ 0 b) (/ 1 x)))
      0))
(define dividend (make-list 1000 #f))
(define stdlib-input (range 64))
(define backquote-input
  '(a (b (unquote c)) (unquote-splicing d) e))
(define backquote-expected
  '(append
    (list (quote a))
    (list (append (list (quote b)) (list c) (quote ())))
    d
    (list (quote e))
    (quote ())))

(define bq-quote (gensym 'bq-quote))
(define bq-list (gensym 'bq-list))
(define bq-append (gensym 'bq-append))

(define (atom? value) (not (pair? value)))

(define (reverse-append values tail)
  (foldl cons tail values))

(define (bq-completely-process value)
  (bq-remove-tokens (bq-process value)))

(define (bq-loop p q)
  (if (atom? p)
      (cons bq-append
            (reverse-append q (list (list bq-quote p))))
      (cond
        [(eq? (car p) 'unquote)
         (if (null? (cddr p))
             (cons bq-append
                   (reverse-append q (list (cadr p))))
             (error "malformed unquote"))]
        [(eq? (car p) 'unquote-splicing)
         (error "dotted unquote-splicing")]
        [else (bq-loop (cdr p) (cons (bracket (car p)) q))])))

(define (bq-process value)
  (cond
    [(atom? value) (list bq-quote value)]
    [(eq? (car value) 'backquote)
     (bq-process (bq-completely-process (cadr value)))]
    [(eq? (car value) 'unquote) (cadr value)]
    [(eq? (car value) 'unquote-splicing)
     (error "unquote-splicing after backquote")]
    [else (bq-loop value '())]))

(define (bracket value)
  (cond
    [(atom? value) (list bq-list (bq-process value))]
    [(eq? (car value) 'unquote)
     (list bq-list (cadr value))]
    [(eq? (car value) 'unquote-splicing) (cadr value)]
    [else (list bq-list (bq-process value))]))

(define (bq-remove-tokens value)
  (cond
    [(eq? value bq-list) 'list]
    [(eq? value bq-append) 'append]
    [(eq? value bq-quote) 'quote]
    [(atom? value) value]
    [else
     (cons (bq-remove-tokens (car value))
           (bq-remove-tokens (cdr value)))]))

(define (stdlib-list)
  (define mapped (map add1 stdlib-input))
  (length
   (filter (lambda (value) (not (< value 32)))
           (append mapped (reverse mapped)))))

(define route-mismatch-tag
  (make-continuation-prompt-tag 'route-mismatch))
(define route-found-tag
  (make-continuation-prompt-tag 'route-found))
(define route-patterns
  '(("GET" "")
    ("GET" "index.js")
    ("POST" "eval")
    ("GET" "git" repo ref)
    ("POST" "git" repo "git-upload-pack")
    ("OPTIONS" "git" repo extra)
    ("GET" "api" "health")
    ("GET" "git" repo "info" "refs")))
(define route-hit-input '("GET" "git" "alice" "info" "refs"))
(define route-miss-input '("DELETE" "missing" "resource"))

(define (send! tag value)
  (call-with-composable-continuation
   (lambda (continuation)
     (abort-current-continuation tag value continuation))
   tag))

(define (handle tag thunk handler)
  (call-with-continuation-prompt thunk tag handler))

(define (safe-car value)
  (if (pair? value) (car value) '()))

(define (safe-cdr value)
  (if (pair? value) (cdr value) '()))

(define (match-route pattern parts acc)
  (if (and (null? pattern) (null? parts))
      (reverse acc)
      (let ([a-head (safe-car pattern)]
            [b-head (safe-car parts)]
            [a-tail (safe-cdr pattern)]
            [b-tail (safe-cdr parts)])
        (cond
          [(not (eq? (null? a-tail) (null? b-tail)))
           (send! route-mismatch-tag (list pattern parts acc))]
          [(equal? a-head b-head)
           (match-route a-tail b-tail acc)]
          [(and (symbol? a-head) (string? b-head))
           (match-route a-tail b-tail (cons b-head acc))]
          [else
           (send! route-mismatch-tag (list pattern parts acc))]))))

(define (route-search parts)
  (handle
   route-found-tag
   (lambda ()
     (for-each
      (lambda (pattern)
        (handle
         route-mismatch-tag
         (lambda ()
           (send! route-found-tag
                  (match-route pattern parts '())))
         (lambda (value continuation) (void))))
      route-patterns)
     'not-found)
   (lambda (bindings continuation) bindings)))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (deriv value)
  (cond
    [(not (pair? value)) (if (eq? value 'x) 1 0)]
    [(or (eq? (car value) '+) (eq? (car value) '-))
     (cons (car value) (map deriv (cdr value)))]
    [(eq? (car value) '*)
     (list '* value
           (cons '+
                 (map (lambda (argument)
                        (list '/ (deriv argument) argument))
                      (cdr value))))]
    [(eq? (car value) '/)
     (list '-
           (list '/ (deriv (cadr value)) (caddr value))
           (list '/ (cadr value)
                 (list '* (caddr value) (caddr value)
                       (deriv (caddr value)))))]
    [else (error "no derivation method available")]))

(define (diviter value)
  (let loop ([value value] [result '()])
    (if (null? value)
        result
        (loop (cddr value) (cons (car value) result)))))

(define (divrec value)
  (if (null? value)
      '()
      (cons (car value) (divrec (cddr value)))))

(define (run-once name)
  (cond
    [(equal? name "tak") (apply tak tak-input)]
    [(equal? name "deriv") (deriv deriv-input)]
    [(equal? name "diviter") (diviter dividend)]
    [(equal? name "divrec") (divrec dividend)]
    [(equal? name "stdlib-list") (stdlib-list)]
    [(equal? name "backquote")
     (bq-completely-process backquote-input)]
    [(equal? name "router-hit") (route-search route-hit-input)]
    [(equal? name "router-miss") (route-search route-miss-input)]
    [else (error "unknown benchmark" name)]))

(define (correct? name result)
  (cond
    [(equal? name "tak") (= result 7)]
    [(equal? name "deriv") (equal? result deriv-expected)]
    [(or (equal? name "diviter") (equal? name "divrec"))
     (= (length result) 500)]
    [(equal? name "stdlib-list") (= result 66)]
    [(equal? name "backquote") (equal? result backquote-expected)]
    [(equal? name "router-hit") (equal? result '("alice"))]
    [(equal? name "router-miss") (eq? result 'not-found)]
    [else #f]))

(define arguments (current-command-line-arguments))
(define name (vector-ref arguments 0))
(define iterations (string->number (vector-ref arguments 1)))
(define warmup
  (if (> (vector-length arguments) 2)
      (string->number (vector-ref arguments 2))
      0))

(when (< iterations 1) (error "iterations must be positive"))

(for ([i (in-range warmup)]) (run-once name))
(define started (current-inexact-monotonic-milliseconds))
(define result
  (for/fold ([result #f]) ([i (in-range iterations)])
    (run-once name)))
(define elapsed-ns
  (inexact->exact
   (round (* 1000000
             (- (current-inexact-monotonic-milliseconds)
                started)))))

(unless (correct? name result) (error "wrong result" name))
(write-json
 (hasheq 'benchmark name
         'iterations iterations
         'elapsed_ns elapsed-ns
         'ns_per_iteration (quotient elapsed-ns iterations)))
(newline)
