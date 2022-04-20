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

(defun authentication-error! ()
  (send! :respond (response 401 () "Unauthorized\n")))

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

(defun add-header! (headers key value)
  (js-call headers "append" key value))

(defun drop (n xs)
  (if (eq? n 0)
      xs
    (drop (- n 1) (tail xs))))

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

(defun cgi-headers (req path-prefix-drop-count)
  (append
   (list "REQUEST_METHOD" (request-method req)
         "QUERY_STRING" (if (post-request? req) ""
                          (request-query-string req))
         "PATH_INFO" (drop-path-prefix path-prefix-drop-count req))
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

(defvar *env* (make-parameter 'env nil))

(defun cgi (req path-drop-count cmd)
  (let* ((cgi-env (append (cgi-headers req path-drop-count)
                          (parameter *env*)))
         (process (js-call <deno> "run"
                    (js-object "cwd" (parameter *cwd*)
                               "env" (apply #'js-object cgi-env)
                               "cmd" (vector-from-list cmd)
                               "stdin" (if (post-request? req) "piped" nil)
                               "stdout" "piped")))
         (stdout-reader (new <buffered-reader> (js-get process "stdout"))))
    (when (post-request? req)
      (pipe-stream! (request-body req)
                    (process-stdin process)))
    (let* ((headers (new <headers>)))
      (cgi-read-headers! headers stdout-reader)
      (new <response> (reader-stream stdout-reader)
           (js-object "headers" headers)))))

(defun add-cors-headers! (response)
  (let ((headers (js-get response "headers")))
    (do (add-header! headers "Access-Control-Allow-Origin" "*")
        (add-header! headers "Access-Control-Allow-Credentials" "true"))))

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
