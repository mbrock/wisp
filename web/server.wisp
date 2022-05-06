;;; * wisp.town git hosting

(load "http.wisp")
(load "auth.wisp")

(defvar *reader*
  (new (js-get <text-proto-reader> "TextProtoReader")
       (new <buffered-reader>
            (js-get <deno> "stdin"))))

(defun read-from-stdin ()
  (list (read-from-string (await (js-call *reader* "readLine")))))

(defun git-http-backend-cgi (repo &optional user-key)
  (binding ((*env* (list "WISP_USER_KEY" (or user-key "")
                         "REMOTE_USER" "git"
                         "REMOTE_ADDR" "wisp.town"
                         "GIT_HTTP_EXPORT_ALL" "1"
                         "GIT_PROJECT_ROOT" "."))
            (*cwd* (string-append "git/" repo)))
    (add-cors-headers!)
    (print *env*)
    (cgi 2 '("git" "http-backend"))))

(defroute ("POST" "git")
  (with-authentication (user-key)
    (let* ((repo-key (symbol-name (genkey!)))
           (repo-path (string-append "git/" repo-key)))
      (mkdir-recursive! repo-path)
      (binding ((*cwd* repo-path))
        (run-command! "git" "init" "--bare")
        (run-command! "git" "config" "core.hooksPath" "../../git-hooks")
        (run-command! "git" "config" "wisp.auth.push" user-key)
        (run-command! "git" "symbolic-ref" "HEAD" "refs/heads/master")
        )
      (add-cors-headers!)
      (set-response-body! repo-key))))

(defroute ("GET" "git" repo "info" "refs")
  (git-http-backend-cgi repo))

(defroute ("GET" "git" repo ref)
  (git-http-backend-cgi repo))

(defroute ("POST" "git" repo "git-receive-pack")
  (with-authentication (user-key)
    (git-http-backend-cgi repo user-key)))

(defroute ("POST" "git" repo "git-upload-pack")
  (git-http-backend-cgi repo))

(defun serve-file (path type)
  (add-header! "content-type" type)
  (let ((file (js-call <deno> "openSync" path)))
    (add-header! "content-length" (js-get (js-call file "statSync") "size"))
    (set-response-body! (reader-stream file))))

(defroute ("GET" "")
  (serve-file "index.html" "text/html"))
(defroute ("GET" "index.js")
  (serve-file "index.js" "text/javascript"))
(defroute ("GET" "index.css")
  (serve-file "index.css" "text/css"))
(defroute ("GET" "dist" "wisp.wasm")
  (serve-file "dist/wisp.wasm" "application/wasm"))
(defroute ("GET" "lib" "idom.js")
  (serve-file "lib/idom.js" "text/javascript"))
(defroute ("GET" "lib" "codemirror.js")
  (serve-file "lib/codemirror.js" "text/javascript"))
(defroute ("GET" "lib" "git.js")
  (serve-file "lib/git.js" "text/javascript"))
(defroute ("GET" "lib" "wisplang.js")
  (serve-file "lib/wisplang.js" "text/javascript"))
(defroute ("GET" "wisp.js")
  (serve-file "wisp.js" "text/javascript"))
(defroute ("GET" "wasi.js")
  (serve-file "wasi.js" "text/javascript"))
(defroute ("GET" "js.wisp")
  (serve-file "js.wisp" "application/wisp"))
(defroute ("GET" "dexp.wisp")
  (serve-file "dexp.wisp" "application/wisp"))
(defroute ("GET" "demo.wisp")
  (serve-file "demo.wisp" "application/wisp"))

(defun preflight ()
  (add-cors-headers!)
  (set-response-code! 204))

(defroute ("OPTIONS" "git") (preflight))
(defroute ("OPTIONS" "git" repo _) (preflight))
(defroute ("OPTIONS" "git" repo _ _) (preflight))

(defun iterator-values (it &optional acc)
  (let ((next (js-call it "next")))
    (if (js-get next "done")
        (reverse acc)
      (iterator-values it (cons (js-get next "value") acc)))))

(defun cgi-fix-header-name (name)
  (cond
    ((equal? name "HTTP_CONTENT_TYPE")
     "CONTENT_TYPE")
    ((equal? name "HTTP_CONTENT_LENGTH")
     "CONTENT_LENGTH")
    (t name)))

(defroute ("POST" "eval")
  (with-authentication (user-key)
    (unless (equal? user-key "~20220405.DAJC4YMX9R")
      (authentication-error!))
    (let* ((code (request-text))
           (value (eval (read-from-string code))))
      (set-response-body!
       (string-append
        (cond
          ((request-accepts? req "text/html")
           (render-sexp-to-html-string value))
          (t (print-to-string value)))
        "\n")))))



(serve 443 "./crt.pem" "./key2.pem"  #'route-request)
(repl)
