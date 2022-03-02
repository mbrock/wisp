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
    (assert (eq *x* 2))
    ))
