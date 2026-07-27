;; -*- mode: wisp; fill-column: 64; -*-
;;
;; Small program benchmarks adapted from the Gabriel benchmarks as
;; collected by the R7RS benchmark suite:
;; https://github.com/ecraven/r7rs-benchmarks
;;
;; The benchmark harness supplies the repetition count.  Fixed
;; program inputs are kept here so the workload is visible and
;; comparable with ports to other implementations.



;;; TAK -- The Takeuchi function.
;;;
;;; This uses an older Gabriel input, (18 12 6), whose expected
;;; result is 7.  The current R7RS input is too large for a useful
;;; first interpreter benchmark.

(defun %gabriel-tak (x y z)
  (if (not (< y x))
      z
      (%gabriel-tak
       (%gabriel-tak (- x 1) y z)
       (%gabriel-tak (- y 1) z x)
       (%gabriel-tak (- z 1) x y))))

(defun %benchmark-tak (count)
  (if (eq? count 1)
      (%gabriel-tak 18 12 6)
      (do
       (%gabriel-tak 18 12 6)
       (%benchmark-tak (- count 1)))))



;;; DERIV -- Symbolic differentiation.
;;;
;;; As in the Gabriel source, the quotient case is intentionally
;;; wrong but is not reached by the benchmark input.

(defun %gabriel-deriv (a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (head a) '+)
         (cons '+
               (map #'%gabriel-deriv (tail a))))
        ((eq? (head a) '-)
         (cons '-
               (map #'%gabriel-deriv (tail a))))
        ((eq? (head a) '*)
         (list '*
               a
               (cons '+
                     (map
                      (fn (a)
                        (list '/
                              (%gabriel-deriv a)
                              a))
                      (tail a)))))
        ((eq? (head a) '/)
         (list '-
               (list '/
                     (%gabriel-deriv (second a))
                     (third a))
               (list '/
                     (second a)
                     (list '*
                           (third a)
                           (third a)
                           (%gabriel-deriv (third a))))))
        (t
         (error "no derivation method available"))))

(defun %benchmark-deriv-one ()
  (%gabriel-deriv
   '(+ (* 3 x x) (* a x x) (* b x) 5)))

(defun %benchmark-deriv (count)
  (if (eq? count 1)
      (%benchmark-deriv-one)
      (do
       (%benchmark-deriv-one)
       (%benchmark-deriv (- count 1)))))



;;; DIVITER and DIVREC -- Divide a list of 1000 NILs by two.
;;;
;;; The input list is constructed during setup, outside the timed
;;; region, matching the R7RS harness.  Both variants return a list
;;; of length 500.

(defun %gabriel-create-n (n)
  (if (eq? n 0)
      nil
      (cons nil (%gabriel-create-n (- n 1)))))

(defvar *gabriel-dividend* (%gabriel-create-n 1000))

(defun %gabriel-diviter-loop (list result)
  (if (nil? list)
      result
      (%gabriel-diviter-loop
       (tail (tail list))
       (cons (head list) result))))

(defun %gabriel-diviter (list)
  (%gabriel-diviter-loop list nil))

(defun %benchmark-diviter (count)
  (if (eq? count 1)
      (%gabriel-diviter *gabriel-dividend*)
      (do
       (%gabriel-diviter *gabriel-dividend*)
       (%benchmark-diviter (- count 1)))))

(defun %gabriel-divrec (list)
  (if (nil? list)
      nil
      (cons (head list)
            (%gabriel-divrec (tail (tail list))))))

(defun %benchmark-divrec (count)
  (if (eq? count 1)
      (%gabriel-divrec *gabriel-dividend*)
      (do
       (%gabriel-divrec *gabriel-dividend*)
       (%benchmark-divrec (- count 1)))))
