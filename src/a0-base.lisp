(set-symbol-function
 'DEFUN
 (%macro-lambda (name args body)
                (list 'set-symbol-function (list 'quote name)
                      (list 'lambda args body))))

(set-symbol-function
 'DEFMACRO
 (%macro-lambda (name args body)
                (list 'set-symbol-function (list 'quote name)
                      (list '%macro-lambda args body))))

(defmacro defvar (var val)
  (list 'set-symbol-value (list 'quote var) val))

(defmacro setq (var val)
  (progn var (list 'set-symbol-value (list 'quote var) val)))

(defun not (x)
  (if x nil t))

(defun equal (x y)
  (if (eq x y) t
      (let ((xt (type-of x))
            (yt (type-of y)))
        (if (not (eq xt yt))
            nil
            (if (eq xt 'cons)
                (equal-lists x y)
                (error xt))))))

(defun equal-lists (x y)
  (if (eq x nil)
      (eq y nil)
      (if (eq y nil)
          nil
          (if (equal (car x) (car y))
              (equal-lists (cdr x) (cdr y))
              nil))))

(defmacro assert (x)
  (list 'if x nil (list 'error (list 'quote x))))

(defmacro with-trace (body)
  (list 'progn '(wtf t) (list 'prog1 body '(wtf nil))))

(defun base-test ()
  (progn
    (assert (equal 1 1))
    (assert (equal '(1 2 3) '(1 2 3)))
    (assert (equal '((1 2) (3 4)) '((1 2) (3 4))))
    (assert (not (equal '(1) '(1 2))))

    (defvar *x* 1)
    (assert (eq *x* 1))
    (setq *x* 2)
    (assert (eq *x* 2))))

(defun test-call/cc ()
  (progn
    (defvar *plusser* nil)
    (progn
      (call/cc (lambda (break)
                 (progn
                   (+ 10 (call/cc (lambda (k)
                                    (progn
                                      (setq *plusser* k)
                                      (funcall break nil))))))))
      (assert (eq 11 (funcall *plusser* 1))))))

(defun null (x)
  (eq x nil))

(defun mapcar (f xs)
  (if (null xs) nil
      (cons (funcall f (car xs))
            (mapcar f (cdr xs)))))

(defun append (xs ys)
  (if (null xs) ys
      (cons (car xs)
            (append (cdr xs) ys))))

(defun last (xs)
  (if (null xs) nil
      (if (null (cdr xs)) xs
          (last (cdr xs)))))

(defun reduce (f xs)
  (if (null xs)
      (funcall f)
      (funcall f (car xs) (reduce f (cdr xs)))))

(defun remove-if (f xs)
  (if (null xs) nil
      (if (funcall f (car xs))
          (remove-if f (cdr xs))
          (cons (car xs) (remove-if f (cdr xs))))))

(defun some (f xs)
  (if (null xs) nil
      (if (funcall f (car xs))
          t
          (some f (cdr xs)))))

(defun butlast (xs)
  (if (null xs) nil
      (if (null (cdr xs)) nil
          (cons (car xs)
                (butlast (cdr xs))))))

(defun unquote? (x)
  (if (eq 'cons (type-of x))
      (eq 'unquote (car x))
      nil))

(defun %quasiquote (x)
  (let ((type (type-of x)))
    (if (eq type 'cons)
        (if (eq 'unquote (car x))
            (car (cdr x))
            (cons 'list
                  (mapcar #'%quasiquote x)))
        (list 'quote x))))

(defmacro quasiquote (x)
  (%quasiquote x))

;; (defmacro defstruct (name &rest fields)

;;   )
