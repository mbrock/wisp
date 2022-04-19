(defvar +jwks-url+
  "https://dev-wnks73rd.us.auth0.com/.well-known/jwks.json")
(defvar +jwt-issuer+ "https://dev-wnks73rd.us.auth0.com/")
(defvar +jwt-audience+ "https://api.wisp.town")
(defvar +jwt-params+ (js-object "issuer" +jwt-issuer+
                                "audience" +jwt-audience+))

(defvar +liberal-cors-headers+
  '("Access-Control-Allow-Origin" "*"
    "Access-Control-Allow-Methods" "POST, GET, OPTIONS"
    "Access-Control-Allow-Credentials" "true"
    "Access-Control-Allow-Headers" "Content-Type, Authorization"))

(defvar <url> (js-get *window* "URL"))
(defvar <response> (js-get *window* "Response"))
(defvar <deno> (js-get *window* "Deno"))
(defvar <headers> (js-get *window* "Headers"))

(defun getenv (v)
  (js-call (js-get <deno> "env") "get" v))

(defun deno-import-module (path)
  (await
   (js-call *wisp* "import" path)))

(defun %deno-import (clauses)
  (map (fn (clause)
         `(defvar ,(head clause)
            (deno-import-module ,(second clause))))
       clauses))

(defmacro deno-import (&rest clauses)
  `(do ,@(%deno-import clauses)))

(deno-import
 (<fs> "https://deno.land/std@0.135.0/fs/mod.ts")
 (<server> "https://deno.land/std@0.135.0/http/server.ts")
 (<buffer> "https://deno.land/std@0.135.0/io/buffer.ts")
 (<conversion> "https://deno.land/std@0.135.0/streams/conversion.ts")
 (<jose> "https://deno.land/x/jose@v4.6.0/index.ts"))

(defvar <buffered-reader> (js-get <buffer> "BufReader"))

(defvar *jwks*
  (js-call <jose> "createRemoteJWKSet"
    (new <url> +jwks-url+)))

(defun serve (port handler)
  (await
   (returning
       (js-call <server> "serve"
         (callback (req)
           (async
            (fn ()
              (let ((response
                        (try (with-simple-error-handler ()
                               (call-with-prompt :respond
                                   (fn () (call handler req))
                                 (fn (v k) v)))
                          (catch (e k)
                            (response 500 '() "oops")))))
                (returning response
                  (print `(status ,(js-get response "status"))))))))
         (js-object "port" port))
     (print `(http server port ,port)))))

(defun request-header (req header)
  (js-call (js-get req "headers") "get" header))

(defun request-text (req)
  (await (js-call req "text")))

(defun bearer-token (authorization)
  (let ((parts (split-string authorization " ")))
    (if (eq? 2 (length parts))
        (second parts)
      nil)))

(defun await-call (object method &rest args)
  (let ((result (apply #'js-call `(,object ,method ,@args))))
    (if (promise? result)
        (await result)
      result)))

(defun authentication-error! ()
  (send! :respond (response 401 () "Unauthorized\n")))

(defun jwt-verify (jwt)
  (try
    (let ((result
            (await-call <jose> "jwtVerify"
                        jwt *jwks* +jwt-params+)))
      (js-get result "payload"))
    (catch (e)
      (authentication-error!))))

(defun jwt-authenticate! (req)
  (let ((result (jwt-verify
                 (or (bearer-token
                      (or (request-header req "authorization")
                          (authentication-error!)))
                     (authentication-error!)))))
    (js-get* result '("https://wisp.town" "key"))))

(defun authenticate! (req f)
  (call f (jwt-authenticate! req)))

(defun string-drop (n s)
  (string-slice s n (string-length s)))

(defun parse-request (req)
  (cons (js-get req "method")
        (split-string
         (string-drop 1 (js-get (new <url> (js-get req "url"))
                                "pathname"))
         "/")))

(defvar *routes* nil)

(defun install-route (pattern handler)
  (set! *routes* (cons (list pattern handler) *routes*)))

(defun match-route (pattern parts acc)
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
         (match-route a-tail b-tail acc))
        ((and (symbol? a-head) (string? b-head))
         (match-route a-tail b-tail (cons b-head acc)))
        (t
         (send! 'route-mismatch (list pattern parts acc)))))))

(defun route-request (req)
  (let* ((parts (parse-request req)))
    (print `(request ,(js-get req "url") ,parts))
    (for-each *routes*
      (fn (route)
        (let ((pattern (head route))
              (handler (second route)))
          (handle
              (let* ((bindings (match-route pattern parts ())))
                (send! :respond (apply handler (cons req bindings))))
            (route-mismatch (v k) nil)))))
    (response 404 () "nope\n")))

(defun request-accept-types (req)
  (split-string (or (request-header req "accept") "text/plain") ", "))

(defun request-accepts? (req type)
  (find (request-accept-types req) (fn (x) (equal? x type))))


(defun copy-file! (src dst)
  (await (js-call <fs> "copy" src dst)))


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



;; we want to be our own git repository backend
;;
;; all we need to do is just call git with cgi
;;
;; let's see what requests git makes
;;

(defun add-header! (headers key value)
  (js-call headers "append" key value)
  )

(defun cgi-read-headers! (headers reader)
  (let ((line (await (js-call reader "readString" "\n"))))
    (when (> (string-length line) 2)
      (let* ((parts (split-string line ": "))
             (header (head parts))
             (value
               (string-slice (second parts)
                             0
                             (- (string-length (second parts)) 2))))
        (add-header! headers header value)
        (cgi-read-headers! headers reader)))))

(defun drop (n xs)
  (if (eq? n 0)
      xs
    (drop (- n 1) (tail xs))))

(defun drop-path-prefix (n req)
  (string-append "/"
                 (join-strings
                  "/"
                  (drop n (tail (split-string
                                 (js-get (new <url> (js-get req "url"))
                                         "pathname")
                                 "/"))))))

(defun request-url (req)
  (new <url> (js-get req "url")))

(defun request-query-string (req)
  (string-drop 1 (js-get (request-url req) "search")))

(defun request-method (req)
  (js-get req "method"))

(defun post-request? (req)
  (equal? (request-method req) "POST"))

(defun git-cgi-headers (req &optional user-key)
  (append
   (list "REQUEST_METHOD" (request-method req)
         "QUERY_STRING" (if (post-request? req) ""
                          (request-query-string req))
         "WISP_USER_KEY" (or user-key "")
         "REMOTE_USER" "git"
         "REMOTE_ADDR" "wisp.town"
         "GIT_HTTP_EXPORT_ALL" "1"
         "GIT_PROJECT_ROOT" "."
         "PATH_INFO" (drop-path-prefix 2 req))
   (apply #'append
          (map (fn (entry)
                 (list (cgi-fix-header-name
                        (string-append
                         "HTTP_"
                         (string-to-uppercase
                          (join-strings
                           "_"
                           (split-string (vector-get entry 0) "-")))))
                       (vector-get entry 1)))
               (iterator-values (js-call (js-get req "headers")
                                    "entries"))))))

(defun pipe-stream! (src dst)
  (js-call src "pipeTo" dst))

(defun request-body (req)
  (js-get req "body"))

(defun process-stdin (process)
  (js-get* process '("stdin" "writable")))

(defun reader-stream (reader)
  (js-call <conversion> "readableStreamFromReader" reader))

(defun git-cgi-exec (req repo &optional user-key)
  (let* ((cgi-headers (git-cgi-headers req user-key))
         (process (js-call <deno> "run"
                    (js-object "cwd" (string-append "git/" repo)
                               "env" (apply #'js-object cgi-headers)
                               "cmd" (vector-from-list '("git" "http-backend"))
                               "stdin" (if (post-request? req) "piped" nil)
                               "stdout" "piped"
                               )))
         (stdout-reader (new <buffered-reader> (js-get process "stdout"))))
    (when (post-request? req)
      (pipe-stream! (request-body req)
                    (process-stdin process)))
    (let* ((headers (new <headers>)))
      (cgi-read-headers! headers stdout-reader)
      (add-header! headers "Access-Control-Allow-Origin" "*")
      (add-header! headers "Access-Control-Allow-Credentials" "true")
      (new <response> (reader-stream stdout-reader)
           (js-object "headers" headers)))))

(defroute ("GET" "git" repo "info" "refs") req
  (git-cgi-exec req repo))

(defroute ("GET" "git" repo ref) req
  (git-cgi-exec req repo))

(defroute ("POST" "git" repo "git-receive-pack") req
  (with-authentication (req user-key)
    (git-cgi-exec req repo user-key)))

(defroute ("POST" "git" repo "git-upload-pack") req
  (git-cgi-exec req repo))

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
