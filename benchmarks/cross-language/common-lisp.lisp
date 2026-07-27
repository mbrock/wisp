(declaim (optimize (speed 3) (safety 1) (debug 0)))

(defparameter *tak-input* '(18 12 6))
(defparameter *deriv-input*
  '(+ (* 3 x x) (* a x x) (* b x) 5))
(defparameter *deriv-expected*
  '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
      (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
      (* (* b x) (+ (/ 0 b) (/ 1 x)))
      0))
(defparameter *dividend* (make-list 1000))
(defparameter *stdlib-input*
  (loop for value below 64 collect value))
(defparameter *backquote-input*
  '(a (b (unquote c)) (unquote-splicing d) e))
(defparameter *backquote-expected*
  '(append
    (list (quote a))
    (list (append (list (quote b)) (list c) (quote nil)))
    d
    (list (quote e))
    (quote nil)))

(defparameter *bq-quote* (gensym "BQ-QUOTE"))
(defparameter *bq-list* (gensym "BQ-LIST"))
(defparameter *bq-append* (gensym "BQ-APPEND"))

(defun atom-p (value) (not (consp value)))

(defun reverse-append (values tail)
  (reduce (lambda (result value) (cons value result))
          values :initial-value tail))

(defun bq-completely-process (value)
  (bq-remove-tokens (bq-process value)))

(defun bq-loop (p q)
  (if (atom-p p)
      (cons *bq-append*
            (reverse-append q (list (list *bq-quote* p))))
      (cond
        ((eq (car p) 'unquote)
         (if (null (cddr p))
             (cons *bq-append*
                   (reverse-append q (list (cadr p))))
             (error "malformed unquote")))
        ((eq (car p) 'unquote-splicing)
         (error "dotted unquote-splicing"))
        (t (bq-loop (cdr p) (cons (bracket (car p)) q))))))

(defun bq-process (value)
  (cond
    ((atom-p value) (list *bq-quote* value))
    ((eq (car value) 'backquote)
     (bq-process (bq-completely-process (cadr value))))
    ((eq (car value) 'unquote) (cadr value))
    ((eq (car value) 'unquote-splicing)
     (error "unquote-splicing after backquote"))
    (t (bq-loop value nil))))

(defun bracket (value)
  (cond
    ((atom-p value) (list *bq-list* (bq-process value)))
    ((eq (car value) 'unquote)
     (list *bq-list* (cadr value)))
    ((eq (car value) 'unquote-splicing) (cadr value))
    (t (list *bq-list* (bq-process value)))))

(defun bq-remove-tokens (value)
  (cond
    ((eq value *bq-list*) 'list)
    ((eq value *bq-append*) 'append)
    ((eq value *bq-quote*) 'quote)
    ((atom-p value) value)
    (t
     (cons (bq-remove-tokens (car value))
           (bq-remove-tokens (cdr value))))))

(defun stdlib-list ()
  (let* ((mapped (mapcar #'1+ *stdlib-input*))
         (mirrored (append mapped (reverse mapped)))
         (large (remove-if (lambda (value) (< value 32))
                           mirrored)))
    (length large)))

(defun tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x))
      z
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))

(defun deriv (value)
  (cond
    ((not (consp value)) (if (eq value 'x) 1 0))
    ((or (eq (car value) '+) (eq (car value) '-))
     (cons (car value) (mapcar #'deriv (cdr value))))
    ((eq (car value) '*)
     (list '* value
           (cons '+
                 (mapcar
                  (lambda (argument)
                    (list '/ (deriv argument) argument))
                  (cdr value)))))
    ((eq (car value) '/)
     (list '-
           (list '/ (deriv (cadr value)) (caddr value))
           (list '/ (cadr value)
                 (list '* (caddr value) (caddr value)
                       (deriv (caddr value))))))
    (t (error "no derivation method available"))))

(defun diviter (value)
  (labels ((walk (value result)
             (if (null value)
                 result
                 (walk (cddr value) (cons (car value) result)))))
    (walk value nil)))

(defun divrec (value)
  (if (null value)
      nil
      (cons (car value) (divrec (cddr value)))))

(defun run-once (name)
  (cond
    ((string= name "tak") (apply #'tak *tak-input*))
    ((string= name "deriv") (deriv *deriv-input*))
    ((string= name "diviter") (diviter *dividend*))
    ((string= name "divrec") (divrec *dividend*))
    ((string= name "stdlib-list") (stdlib-list))
    ((string= name "backquote")
     (bq-completely-process *backquote-input*))
    (t (error "unknown benchmark: ~A" name))))

(defun correct-p (name result)
  (cond
    ((string= name "tak") (= result 7))
    ((string= name "deriv") (equal result *deriv-expected*))
    ((or (string= name "diviter") (string= name "divrec"))
     (= (length result) 500))
    ((string= name "stdlib-list") (= result 66))
    ((string= name "backquote")
     (equal result *backquote-expected*))
    (t nil)))

(let* ((arguments sb-ext:*posix-argv*)
       (name (nth 1 arguments))
       (iterations (parse-integer (nth 2 arguments)))
       (warmup (if (nth 3 arguments)
                   (parse-integer (nth 3 arguments))
                   0)))
  (when (< iterations 1) (error "iterations must be positive"))
  (dotimes (i warmup) (declare (ignore i)) (run-once name))
  (let ((started (get-internal-real-time))
        (result nil))
    (dotimes (i iterations)
      (declare (ignore i))
      (setf result (run-once name)))
    (let* ((ticks (- (get-internal-real-time) started))
           (elapsed-ns
             (round (* ticks 1000000000)
                    internal-time-units-per-second)))
      (unless (correct-p name result)
        (error "wrong result for ~A" name))
      (format t
              "{\"benchmark\":\"~A\",\"iterations\":~D,"
              name iterations)
      (format t
              "\"elapsed_ns\":~D,\"ns_per_iteration\":~D}~%"
              elapsed-ns (floor elapsed-ns iterations)))))
