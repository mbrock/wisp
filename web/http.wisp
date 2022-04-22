;;; * HTTP serving via Deno

(defvar <url> (js-get *window* "URL"))
(defvar <response> (js-get *window* "Response"))
(defvar <headers> (js-get *window* "Headers"))

(defvar +liberal-cors-headers+
  '("Access-Control-Allow-Origin" "*"
    "Access-Control-Allow-Methods" "POST, GET, OPTIONS"
    "Access-Control-Allow-Credentials" "true"
    "Access-Control-Allow-Headers" "Content-Type, Authorization"))

(deno-import
 (<server> "https://deno.land/std@0.135.0/http/server.ts"))

(defparameter *request* nil)
(defparameter *response* nil)

(defun response (status headers &optional body)
  (vector status (apply #'js-object headers) body))

(defun set-response-status! (status)
  (vector-set! *response* 0 status))

(defun set-response-body! (body)
  (vector-set! *response* 2 body))

(defun serve (port handler)
  (returning
      (js-call <server> "serve"
        (callback (request)
          (async
           (fn ()
             (binding ((*request* request)
                       (*response* (vector 200 (new <headers>) nil)))
               (try (with-simple-error-handler ()
                      (call-with-prompt :respond
                          (fn () (call handler))
                        (fn (v k) (set! *response* v))))
                 (catch (e k)
                   (set! *response*
                         (vector 500 nil "Internal Server Error"))))
               (returning (new <response> (vector-get *response* 2)
                               (js-object "status" (vector-get *response* 0)
                                          "headers" (vector-get *response* 1)))
                 (print `(status ,(vector-get *response* 0))))))))
        (js-object "port" port))
    (print `(http server port ,port))))

(defun request-header (header)
  (js-call (js-get *request* "headers") "get" header))

(defun request-text ()
  (await (js-call *request* "text")))

(defun bearer-token (authorization)
  (let ((parts (split-string authorization " ")))
    (if (eq? 2 (length parts))
        (second parts)
      nil)))

(defun authentication-error! ()
  (send! :respond (response 401 () "Unauthorized\n")))

(defun string-drop (n s)
  (string-slice s n (string-length s)))

(defun parse-request ()
  (cons (js-get *request* "method")
        (split-string
         (string-drop 1 (js-get (request-url) "pathname"))
         "/")))

(defvar *routes* nil)

(defun install-route (pattern handler)
  (set! *routes* (cons (list pattern handler) *routes*)))

(defmacro defroute (pattern &rest body)
  `(install-route ',pattern (fn (,@(filter pattern #'symbol?))
                              ,(prognify body))))

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

(defun route-request ()
  (let* ((parts (parse-request)))
    (print `(request ,(js-get *request* "url") ,parts))
    (for-each *routes*
      (fn (route)
        (let ((pattern (head route))
              (handler (second route)))
          (handle
              (let* ((bindings (match-route pattern parts ())))
                (send! :respond (returning *response*
                                  (apply handler bindings))))
            (route-mismatch (v k) nil)))))
    (response 404 nil "no route")))

(defun request-accept-types ()
  (split-string (or (request-header *request* "accept") "text/plain") ", "))

(defun request-accepts? (type)
  (find (request-accept-types *request*) (fn (x) (equal? x type))))

(defun drop-path-prefix (n)
  (string-append "/"
                 (join-strings
                  "/"
                  (drop n (tail (split-string
                                 (js-get (request-url) "pathname")
                                 "/"))))))

(defun request-url ()
  (new <url> (js-get *request* "url")))

(defun request-query-string ()
  (string-drop 1 (js-get (request-url) "search")))

(defun request-method ()
  (js-get *request* "method"))

(defun post-request? ()
  (equal? (request-method) "POST"))

(defun add-header! (key value)
  (js-call (response-headers) "append" key value))

(defun drop (n xs)
  (if (eq? n 0)
      xs
    (drop (- n 1) (tail xs))))

(defun cgi-read-headers! (reader)
  (let ((line (await (js-call reader "readString" "\n"))))
    (when (> (string-length line) 2)
      (let* ((parts (split-string line ": "))
             (header (head parts))
             (value
               (string-slice (second parts)
                             0
                             (- (string-length (second parts)) 2))))
        (add-header! header value)
        (cgi-read-headers! reader)))))

(defun cgi-headers (path-prefix-drop-count)
  (append
   (list "REQUEST_METHOD" (request-method)
         "QUERY_STRING" (if (post-request?) ""
                          (request-query-string))
         "PATH_INFO" (drop-path-prefix path-prefix-drop-count))
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
               (iterator-values (js-call (request-headers) "entries"))))))

(defun pipe-stream! (src dst)
  (js-call src "pipeTo" dst))

(defun request-headers ()
  (js-get *request* "headers"))

(defun request-body ()
  (js-get *request* "body"))

(defun process-stdin (process)
  (js-get* process '("stdin" "writable")))

(defun reader-stream (reader)
  (js-call <conversion> "readableStreamFromReader" reader))

(defun cgi (path-drop-count cmd)
  (let* ((cgi-env (append (cgi-headers path-drop-count) *env*))
         (process (js-call <deno> "run"
                    (js-object "cwd" *cwd*
                               "env" (apply #'js-object cgi-env)
                               "cmd" (vector-from-list cmd)
                               "stdin" (if (post-request?) "piped" "null")
                               "stdout" "piped")))
         (stdout-reader (new <buffered-reader> (js-get process "stdout"))))
    (when (post-request?)
      (pipe-stream! (request-body)
                    (process-stdin process)))
    (cgi-read-headers! stdout-reader)
    (set-response-body! (reader-stream stdout-reader))))

(defun response-headers ()
  (vector-get *response* 1))

(defun add-cors-headers! ()
  (let ((headers (response-headers)))
    (do (add-header! "Access-Control-Allow-Origin" "*")
        (add-header! "Access-Control-Allow-Credentials" "true"))))

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
