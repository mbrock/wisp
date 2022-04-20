;;; * wisp.town git hosting

(load "http.wisp")
(load "auth.wisp")

(defun git-http-backend-cgi (req repo &optional user-key)
  (let ((response
            (with *env* (list "WISP_USER_KEY" (or user-key "")
                              "REMOTE_USER" "git"
                              "REMOTE_ADDR" "wisp.town"
                              "GIT_HTTP_EXPORT_ALL" "1"
                              "GIT_PROJECT_ROOT" ".")
                  (with *cwd* (string-append "git/" repo)
                        (cgi req 2 '("git" "http-backend"))))))
    (returning response
      (add-cors-headers! response))))

(defroute ("POST" "git") req
  (with-authentication (req user-key)
    (let* ((repo-key (symbol-name (genkey!)))
           (repo-path (string-append "git/" repo-key)))
      (mkdir-recursive! repo-path)
      (with *cwd* repo-path
            (run-command! "git" "init" "--bare")
            (run-command! "git" "config" "core.hooksPath" "../../git-hooks")
            (run-command! "git" "config" "wisp.auth.push" user-key))
      (response 200 +liberal-cors-headers+ repo-key))))

(defroute ("GET" "git" repo "info" "refs") req
  (git-http-backend-cgi req repo))

(defroute ("GET" "git" repo ref) req
  (git-http-backend-cgi req repo))

(defroute ("POST" "git" repo "git-receive-pack") req
  (with-authentication (req user-key)
    (git-http-backend-cgi req repo user-key)))

(defroute ("POST" "git" repo "git-upload-pack") req
  (git-http-backend-cgi req repo))

(defroute ("OPTIONS" "git" repo "git-upload-pack") req
  (response 204 +liberal-cors-headers+))

(defroute ("OPTIONS" "git" repo "git-receive-pack") req
  (response 204 +liberal-cors-headers+))

(defroute ("OPTIONS" "git" repo "info" anything) req
  (response 204 +liberal-cors-headers+))

(defroute ("OPTIONS" "git") req
  (response 204 +liberal-cors-headers+))

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

(defroute ("POST" "eval") req
  (with-authentication (req user-key)
    (unless (equal? user-key "~20220405.DAJC4YMX9R")
      (authentication-error!))
    (let* ((code (request-text req))
           (value (eval (read-from-string code))))
      (response 200 ()
        (string-append
         (cond
           ((request-accepts? req "text/html")
            (render-sexp-to-html-string value))
           (t (print-to-string value)))
         "\n")))))



(serve 8000 #'route-request)
