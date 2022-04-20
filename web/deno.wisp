(defvar <deno> (js-get *window* "Deno"))

(defun getenv (v)
  (js-call (js-get <deno> "env") "get" v))

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

(deno-import
 (<fs> "https://deno.land/std@0.135.0/fs/mod.ts")
 (<buffer> "https://deno.land/std@0.135.0/io/buffer.ts")
 (<conversion> "https://deno.land/std@0.135.0/streams/conversion.ts"))

(defun copy-file! (src dst)
  (await (js-call <fs> "copy" src dst)))

(defun read-text-file (src)
  (await (js-call <deno> "readTextFile" src)))

(defvar <buffered-reader> (js-get <buffer> "BufReader"))

(defvar *loaded-files* '("deno.wisp"))

(defun load (filename)
  (unless (includes? *loaded-files* filename)
    (print `(loading wisp ,filename))
    (for-each (read-many-from-string
               (read-text-file filename))
      (fn (form)
        (eval form)))
    (set! *loaded-files* (cons filename *loaded-files*))))
