;;; * Local Wisp development server

(load "http.wisp")

(defvar *dev-browsers* nil)
(defvar *dev-replies* nil)

(defun serve-file (path type)
  (add-header! "content-type" type)
  (add-header! "cache-control" "no-store")
  (set-response-body! (await (js-call <deno> "readFile" path))))

(defun forget-dev-browser! (socket)
  (set! *dev-browsers* (remove *dev-browsers* socket))
  (print `(browser disconnected
           ,(length *dev-browsers*) connected)))

(defun accept-dev-browser! ()
  (let* ((upgrade
           (js-call <deno> "upgradeWebSocket" *request*))
         (socket (js-get upgrade "socket")))
    (do
      (js-set! socket "onopen"
        (callback (event)
          (set! *dev-browsers*
                (cons socket *dev-browsers*))
          (print `(browser connected
                   ,(length *dev-browsers*) connected))
          nil))
      (js-set! socket "onmessage"
        (callback (event)
          (set! *dev-replies*
                (cons (js-get event "data")
                      *dev-replies*))
          nil))
      (js-set! socket "onclose"
        (callback (event)
          (forget-dev-browser! socket)
          nil))
      (send! :respond (js-get upgrade "response")))))

(defun send-to-dev-browsers! (source)
  (if (nil? *dev-browsers*)
      (do
        (set-response-status! 503)
        (set-response-body! "no browser connected\n"))
    (do
      (for-each *dev-browsers*
        (fn (socket)
          (js-call socket "send" source)))
      (set-response-status! 202)
      (set-response-body!
       (string-append
        "sent to "
        (print-to-string (length *dev-browsers*))
        " browser(s)\n")))))

(defun drain-dev-replies! ()
  (let ((replies (reverse *dev-replies*)))
    (do
      (set! *dev-replies* nil)
      (add-header! "content-type" "application/wisp")
      (add-header! "cache-control" "no-store")
      (set-response-body!
       (if (nil? replies)
           ""
         (string-append
          (join-strings "\n" replies)
          "\n"))))))

(defroute ("GET" "swank")
  (accept-dev-browser!))

(defroute ("POST" "swank" "eval")
  (add-header! "content-type" "text/plain")
  (add-header! "cache-control" "no-store")
  (send-to-dev-browsers! (request-text)))

(defroute ("GET" "swank" "replies")
  (drain-dev-replies!))

(defroute ("GET" "")
  (serve-file "index.html" "text/html"))
(defroute ("GET" "index.js")
  (serve-file "index.js" "text/javascript"))
(defroute ("GET" "index.css")
  (serve-file "index.css" "text/css"))
(defroute ("GET" "service-worker.js")
  (serve-file "service-worker.js" "text/javascript"))
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

(serve-http 8765 #'route-request)
