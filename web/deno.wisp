(deno-import
 (<fs> "https://deno.land/std@0.135.0/fs/mod.ts")
 (<buffer> "https://deno.land/std@0.135.0/io/buffer.ts")
 (<text-proto-reader> "https://deno.land/std@0.135.0/textproto/mod.ts")
 (<conversion> "https://deno.land/std@0.135.0/streams/conversion.ts"))

(defun copy-file! (src dst)
  (await (js-call <fs> "copy" src dst)))

(defvar <buffered-reader> (js-get <buffer> "BufReader"))

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
  (await (js-call  <deno> "mkdir" path (js-object "recursive" t))))
