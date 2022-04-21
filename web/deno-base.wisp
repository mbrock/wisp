(defvar <deno> (js-get *window* "Deno"))

(defvar *loaded-files* '())

(defun read-text-file (src)
  (await (js-call <deno> "readTextFile" src)))

(defun load (filename)
  (unless (includes? *loaded-files* filename)
    (print `(loading wisp ,filename))
    (let* ((source (read-text-file filename))
           (stream (string-input-stream source)))
      (loop (fn ()
              (let ((form (read stream (fn () (send! :break)))))
                (eval form)))))
    (set! *loaded-files* (cons filename *loaded-files*))))

(defun deno-import-module (path)
  (await
   (js-call *wisp* "import" path)))

(defun %deno-import (clauses)
  (map (fn (clause)
         `(defvar ,(head clause)
            (do
              (print `(loading deno ,,(second clause)))
              (deno-import-module ,(second clause)))))
       clauses))

(defmacro deno-import (&rest clauses)
  `(do ,@(%deno-import clauses)))
