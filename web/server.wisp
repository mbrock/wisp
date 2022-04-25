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
    (cgi 2 '("git" "http-backend"))))

(defroute ("POST" "git")
  (with-authentication (user-key)
    (let* ((repo-key (symbol-name (genkey!)))
           (repo-path (string-append "git/" repo-key)))
      (mkdir-recursive! repo-path)
      (binding ((*cwd* repo-path))
        (run-command! "git" "init" "--bare")
        (run-command! "git" "config" "core.hooksPath" "../../git-hooks")
        (run-command! "git" "config" "wisp.auth.push" user-key))
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



(serve 8000 #'route-request)
(repl)
