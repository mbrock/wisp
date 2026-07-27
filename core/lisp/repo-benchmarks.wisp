;; -*- mode: wisp; fill-column: 64; -*-
;;
;; Benchmarks derived from Wisp's own library and web-serving code.
;; Setup data is constructed before the timed invocation.



;;; Standard-library list pipeline

(defun %benchmark-iota (count result)
  (if (eq? count 0)
      result
      (%benchmark-iota (- count 1)
                       (cons (- count 1) result))))

(defvar *benchmark-list-input* (%benchmark-iota 64 nil))

(defun %benchmark-stdlib-one ()
  (let* ((mapped
           (map (fn (value) (+ value 1))
                *benchmark-list-input*))
         (mirrored (append mapped (reverse mapped)))
         (large
           (remove-if (fn (value) (< value 32))
                      mirrored)))
    (length large)))

(defun %benchmark-stdlib (count)
  (if (eq? count 1)
      (%benchmark-stdlib-one)
      (do
       (%benchmark-stdlib-one)
       (%benchmark-stdlib (- count 1)))))



;;; Boot-core backquote expansion

(defvar *benchmark-backquote-input*
  '(a (b (unquote c)) (unquote-splicing d) e))

(defun %benchmark-backquote-one ()
  (bq-completely-process *benchmark-backquote-input*))

(defun %benchmark-backquote (count)
  (if (eq? count 1)
      (%benchmark-backquote-one)
      (do
       (%benchmark-backquote-one)
       (%benchmark-backquote (- count 1)))))



;;; Continuation-based route matching
;;;
;;; MATCH-ROUTE is kept structurally identical to web/http.wisp.
;;; The surrounding search mirrors ROUTE-REQUEST's inner
;;; ROUTE-MISMATCH prompt and its outer RESPOND prompt, while
;;; replacing Deno request/response objects with fixed data.

(defun %benchmark-match-route (pattern parts acc)
  (if (and (nil? pattern) (nil? parts))
      (reverse acc)
    (let ((a-head (head pattern))
          (b-head (head parts))
          (a-tail (tail pattern))
          (b-tail (tail parts)))
      (cond
        ((not (eq? (nil? a-tail) (nil? b-tail)))
         (send! 'route-mismatch (list pattern parts acc)))
        ((equal? a-head b-head)
         (%benchmark-match-route a-tail b-tail acc))
        ((and (symbol? a-head) (string? b-head))
         (%benchmark-match-route
          a-tail b-tail (cons b-head acc)))
        (t
         (send! 'route-mismatch
                (list pattern parts acc)))))))

(defvar *benchmark-route-patterns*
  '(("GET" "")
    ("GET" "index.js")
    ("POST" "eval")
    ("GET" "git" repo ref)
    ("POST" "git" repo "git-upload-pack")
    ("OPTIONS" "git" repo extra)
    ("GET" "api" "health")
    ("GET" "git" repo "info" "refs")))

(defun %benchmark-route-search (parts)
  (handle
      (do
       (for-each
        *benchmark-route-patterns*
        (fn (pattern)
          (handle
              (let ((bindings
                      (%benchmark-match-route
                       pattern parts nil)))
                (send! 'benchmark-route-found bindings))
            (route-mismatch (value continuation) nil))))
       'not-found)
    (benchmark-route-found (bindings continuation) bindings)))

(defvar *benchmark-route-hit-input*
  '("GET" "git" "alice" "info" "refs"))

(defvar *benchmark-route-miss-input*
  '("DELETE" "missing" "resource"))

(defun %benchmark-router-hit-one ()
  (%benchmark-route-search *benchmark-route-hit-input*))

(defun %benchmark-router-hit (count)
  (if (eq? count 1)
      (%benchmark-router-hit-one)
      (do
       (%benchmark-router-hit-one)
       (%benchmark-router-hit (- count 1)))))

(defun %benchmark-router-miss-one ()
  (%benchmark-route-search *benchmark-route-miss-input*))

(defun %benchmark-router-miss (count)
  (if (eq? count 1)
      (%benchmark-router-miss-one)
      (do
       (%benchmark-router-miss-one)
       (%benchmark-router-miss (- count 1)))))
