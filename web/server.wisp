(defvar +jwks-url+
  "https://dev-wnks73rd.us.auth0.com/.well-known/jwks.json")
(defvar +jwt-issuer+ "https://dev-wnks73rd.us.auth0.com/")
(defvar +jwt-audience+ "https://api.wisp.town")
(defvar +jwt-params+ (js-object "issuer" +jwt-issuer+
                                "audience" +jwt-audience+))

(defvar <url> (js-get *window* "URL"))
(defvar <response> (js-get *window* "Response"))
(defvar <deno> (js-get *window* "Deno"))
(defvar <headers> (js-get *window* "Headers"))

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
              (log (js-get req "url"))
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
    (print `(request ,parts))
    (for-each *routes*
      (fn (route)
        (let ((pattern (head route))
              (handler (second route)))
          (handle
              (do
                (print `(match-route ,pattern ,parts))
                (let* ((bindings (match-route pattern parts ())))
                  (print `(bindings ,bindings))
                  (send! :respond (apply handler (cons req bindings)))))
            (route-mismatch (v k) nil)))))
    (response 404 () "nope\n")))

(defun request-accept-types (req)
  (split-string (or (request-header req "accept") "text/plain") ", "))

(defun request-accepts? (req type)
  (find (request-accept-types req) (fn (x) (equal? x type))))



(defroute ("POST" "git") req
  (with-authentication (req user-key)
    (let* ((repo-key (symbol-name (genkey!)))
           (repo-path (string-append "git/" repo-key)))
      (mkdir-recursive! repo-path)
      (with *cwd* repo-path
        (run-command! "git" "init" "--bare")
        (run-command! "git" "config" "wisp.auth.push" user-key))
      (response 200 ()
        (string-append "https://git.wisp.town/" repo-key)))))



;; we want to be our own git repository backend
;;
;; all we need to do is just call git with cgi
;;
;; let's see what requests git makes
;;

(defun cgi-read-headers (headers reader)
  (let ((line (await (js-call reader "readString" "\n"))))
    (when (> (string-length line) 2)
      (print `(line ,line))
      (let* ((parts (split-string line ": "))
             (header (head parts))
             (value
               (string-slice (second parts)
                             0
                             (- (string-length (second parts)) 2))))
        (print `(header ,header ,value))
        (js-call headers "append" header value)
        (cgi-read-headers headers reader)))))

(defun git-cgi-get (req repo path)
  (let* ((process (js-call <deno> "run"
                    (js-object "cwd" (string-append "git/" repo)
                               "env" (js-object
                                      "REQUEST_METHOD" "GET"
                                      "GIT_HTTP_EXPORT_ALL" "1"
                                      "GIT_PROJECT_ROOT" "."
                                      "PATH_INFO" path)
                               "cmd" (vector-from-list '("git" "http-backend"))
                               "stdout" "piped")))
         (reader (new <buffered-reader> (js-get process "stdout")))
         (headers (new <headers>)))
    (cgi-read-headers headers reader)
    (let* ((stream (js-call <conversion> "readableStreamFromReader" reader)))
      (new <response> stream (js-object "headers" headers)))))

(defroute ("GET" "git" repo "info" "refs") req
  (git-cgi-get req repo "/info/refs"))

(defroute ("GET" "git" repo ref) req
  (git-cgi-get req repo (string-append "/" ref)))

(defroute ("GET" "git" repo "objects" x y) req
  (git-cgi-get req repo (string-append "/objects/" x "/" y)))

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

(defroute ("PROPFIND" "git" repo "") req
  ;; cgi
  (let* ((cgi-headers
           (append
            `("REQUEST_METHOD" "PROPFIND"
                               "QUERY_STRING" ""
                               "REMOTE_USER" "mbrock"
                               "REMOTE_ADDR" "localhost"
                               "GIT_HTTP_EXPORT_ALL" "1"
                               "GIT_PROJECT_ROOT" "."
                               "PATH_INFO" "/")
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
         (process (js-call <deno> "run"
                    (js-object "env" (apply #'js-object cgi-headers)
                               "cwd" (string-append "./git/" repo)
                               "cmd" (vector-from-list '("git" "http-backend"))
                               "stdout" "piped"
                               "stdin" "piped"
                               )))
         (reader (new <buffered-reader> (js-get process "stdout")))
         (headers (new <headers>)))
    (print `(cgi-headers ,cgi-headers))
    (js-call (js-get req "body") "pipeTo" (js-get* process '("stdin" "writable")))
    (cgi-read-headers headers reader)
    (let* ((stream (js-call <conversion> "readableStreamFromReader" reader)))
      (new <response> stream (js-object "headers" headers)))))



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
