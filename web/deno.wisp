(defun setup-deno! ()
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
   (<text-proto-reader> "https://deno.land/std@0.135.0/textproto/mod.ts")
   (<conversion> "https://deno.land/std@0.135.0/streams/conversion.ts"))

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

  (defun copy-file! (src dst)
    (await (js-call <fs> "copy" src dst)))

  (defun read-text-file (src)
    (await (js-call <deno> "readTextFile" src)))

  (defparameter *env* nil)
  (defparameter *cwd* ".")

  (defun run-command! (&rest cmd)
    (print `(run-command! :cwd ,*cwd* :env ,*env* :cmd ,cmd))
    (let* ((process (js-call <deno> "run"
                      (js-object "cwd" *cwd*
                                 "env" (apply #'js-object *env*)
                                 "cmd" (vector-from-list cmd))))
           (status (await (js-call process "status"))))
      (if (eq? 0 (js-get status "code"))
          t
        (error `(command-error (cwd ,*cwd*) (cmd ,cmd))))))

  (defun mkdir-recursive! (path)
    (await (js-call  <deno> "mkdir" path (js-object "recursive" t)))))
