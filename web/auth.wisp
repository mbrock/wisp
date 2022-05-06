(deno-import
 (<jose> "https://deno.land/x/jose@v4.6.0/index.ts"))

(defvar +jwks-url+
  "https://dev-wnks73rd.us.auth0.com/.well-known/jwks.json")
(defvar +jwt-issuer+ "https://dev-wnks73rd.us.auth0.com/")
(defvar +jwt-audience+ "https://api.wisp.town")
(defvar +jwt-params+ (js-object "issuer" +jwt-issuer+
                                "audience" +jwt-audience+))

(defvar *jwks*
  (js-call <jose> "createRemoteJWKSet"
    (new <url> +jwks-url+)))

(defun authentication-error! ()
  (send! :respond (response 401 () "Unauthorized\n")))

(defun jwt-verify (jwt)
  (try
    (let ((result
            (await (js-call <jose> "jwtVerify"
                     jwt *jwks* +jwt-params+))))
      (js-get result "payload"))
    (catch (e)
      (authentication-error!))))

(defun jwt-authenticate! ()
  (let ((result (jwt-verify
                 (or (bearer-token
                      (or (request-header "authorization")
                          (authentication-error!)))
                     (authentication-error!)))))
    (js-get* result '("https://wisp.town" "key"))))

(defun authenticate! (f)
  (call f (jwt-authenticate!)))

(defmacro with-authentication (clause &rest body)
  `(authenticate! (fn (,(head clause)) ,@body)))
