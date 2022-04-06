(defvar +jwks-url+
  "https://dev-wnks73rd.us.auth0.com/.well-known/jwks.json")

(defun main ()
  (defun new (constructor &rest args)
    (apply #'js-new (cons constructor args)))

  (defvar <url> (js-get *window* "URL"))
  (defvar <response> (js-get *window* "Response"))

  (defmacro callback (args &rest body)
    `(make-pinned-value
      (fn ,args ,@body)))

  (defun deno-import-module (path)
    (await
     (js-call *wisp* "import" path)))

  (defun %deno-import (clauses)
    (map (fn (clause)
           `(defvar ,(head clause)
              (deno-import-module ,(second clause))))
         clauses))

  (defmacro deno-import (&rest clauses)
    `(progn ,@(%deno-import clauses)))

  (deno-import
   (<server> "https://deno.land/std@0.133.0/http/server.ts")
   (<jose> "https://deno.land/x/jose@v4.6.0/index.ts"))

  ;; const JWKS = jose.createRemoteJWKSet(
  ;;   new URL("https://dev-wnks73rd.us.auth0.com/.well-known/jwks.json")
  ;; )

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

  (defun response (status headers body)
    (new <response> body
         (js-object "status" status
                    "headers" (apply #'js-object headers))))

  (defun js-get* (object indices)
    (if (nil? indices)
        object
      (js-get* (js-get object (head indices))
               (tail indices))))

  (defun request-header (req header)
    (js-call (js-get req "headers") "get" header))

  (defun bearer-token (authorization)
    (let ((parts (split-string authorization " ")))
      (if (eq? 2 (length parts))
          (second parts)
        nil)))

  ;; const { payload } =
  ;;   await jose.jwtVerify(jwt, jwks, {
  ;;     issuer: "https://dev-wnks73rd.us.auth0.com/",
  ;;     audience: "https://api.wisp.town",
  ;;   })

  ;; const key = (payload["https://wisp.town"] || { key: null }).key
  (defun await-call (object method &rest args)
    (let ((result (apply #'js-call `(,object ,method ,@args))))
      (if (promise? result)
          (await result)
        result)))

  (defvar +jwt-issuer+ "https://dev-wnks73rd.us.auth0.com/")
  (defvar +jwt-audience+ "https://api.wisp.town")
  (defvar +jwt-params+ (js-object "issuer" +jwt-issuer+
                                  "audience" +jwt-audience+))

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

  (serve 8000
    (fn (req)
      (progn
        (print `(http ,(js-get req "method")
                      ,(js-get req "url")))
        (let ((user-key (jwt-authenticate! req)))
          (returning
              (response 200 '("content-type" "text/plain")
                (string-append "hello " user-key "\n"))
            (print (list 'authenticated user-key))))))))

(with-simple-error-handler ()
  (async #'main))
