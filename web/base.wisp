(defvar *window* (js-global-this))
(defvar *wisp* (js-get *window* "wisp"))
(defvar *document* (js-get *window* "document"))
(defvar *console* (js-get *window* "console"))
(defvar <promise> (js-get *window* "Promise"))

(defun js-get* (object indices)
  (if (nil? indices)
      object
    (js-get* (js-get object (head indices))
             (tail indices))))

(defun new (constructor &rest args)
  (apply #'js-new (cons constructor args)))

(defun query-selector (selector &optional root)
  (js-call (or root *document*) "querySelector" selector))

(defun query-selector-all (selector &optional root)
  (print `(query-selector-all ,selector ,root))
  (js-call (or root *document*) "querySelectorAll" selector))

(defun promise? (x)
  (if (and (eq? 'external (type-of x))
           (js-get x "then"))
      t
    nil))

(defun log (x)
  (js-call *console* "log" X))

(defun alert (x)
  (js-call *window* "alert" x))

(defun json (str)
  (js-call (js-get *window* "JSON") "parse" str))

(defun js-then (p f)
  (if (promise? p)
      (js-call p "then" (make-pinned-value f))
    (call f p)))

(defun js-catch (p f)
  (if (promise? p)
      (js-call p "catch" (make-pinned-value f))
    p))

(defun async (f)
  (call-with-prompt :async f
    (fn (v k)
      (js-catch
          (js-then v
            (fn (x)
              (async (fn () (call k x)))))
        (fn (e)
          (log e)
          (nonlocal-error! k e))))))

(defun await (x)
  (send! :async x))

(defmacro fetch (url &rest opts)
  `(await (js-call *window* "fetch" ,url (js-object ,@opts))))

(defun response-text (x)
  (await (js-call x "text")))

(defmacro :article (&rest body)
  `(do ,@body))

(defmacro :section (title &rest body)
  `(do ,@body))

(defun css (clauses)
  (reduce #'string-append
          (map (fn (clause)
                 (string-append
                  (symbol-name (head clause))
                  ": "
                  (let ((value (second clause)))
                    (if (symbol? value)
                        (symbol-name value)
                      (if (integer? value)
                          (string-append
                           (print-to-string value) "px")
                        value))) "; "))
               clauses)
          ""))

(:section (basic binding to incremental dom)
  (defmacro callback (args &rest body)
    `(make-pinned-value
      (fn ,args ,@body)))
  
  (defun make-callback (symbol)
    (callback (&rest args)
      (apply (symbol-function symbol) args)))

  (defmacro tag (tag-symbol attrs &rest body)
    (let ((tag-name-var (fresh-symbol!)))
      `(let* ((,tag-name-var (symbol-name ,tag-symbol)))
         (idom-open-start! ,tag-name-var)
         (for-each ,attrs
           (fn (attr)
             (idom-attr! (symbol-name (head attr))
                         (second attr))))
         (idom-open-end!)
         ,@body
         (idom-close! ,tag-name-var))))

  (defun text (text)
    (idom-text! text)))

(:section (rendering expressions to html)
  (defun render-sexp (sexp)
    (cond
      ((nil? sexp)
       (tag :div '((:class "wisp value list")) nil))
      ((symbol? sexp)
       (tag :span `((:class "wisp value symbol")
                    (:data-package-name
                     ,(package-name
                       (symbol-package sexp)))
                    (:data-symbol-name
                     ,(symbol-name sexp))
                    (:data-function-kind
                     ,(if (symbol-function sexp)
                          (if (jet? (symbol-function sexp))
                              "jet" "fun")
                        "")))
         (cond
           ((eq? sexp 'todo)
            (tag :input '((:type "checkbox")) nil))
           ((eq? sexp 'done)
            (tag :input '((:type "checkbox")
                          (:checked "checked")) nil))
           (t
            (do
              (tag :span '((:class "package-name"))
                (text (package-name (symbol-package sexp))))
              (tag :span '((:class "symbol-name"))
                (text (symbol-name sexp))))))))
      ((pair? sexp)
       (let* ((callee (head sexp))
              (tag-type (cond
                          ((eq? callee :section) :section)
                          ((eq? callee :article) :article)
                          (t :div))))
         (tag tag-type
           `((:class "wisp value list")
             (:data-callee
              ,(if (symbol? callee)
                   (string-append
                    (package-name (symbol-package callee))
                    ":"
                    (symbol-name callee))
                 "")))
           (render-list-contents sexp))))
      ((string? sexp)
       (tag :span '((:class "wisp value string"))
         (text sexp)))
      ((integer? sexp)
       (tag :span '((:class "wisp value number"))
         (text (print-to-string sexp))))
      ((eq? 'vector (type-of sexp))
       (tag :div '((:class "wisp value vector"))
         (render-vector-contents sexp 0)))
      ((eq? 'function (type-of sexp))
       (tag :i ()
         (if (function-name sexp)
             (text (symbol-name (function-name sexp)))
           (text "#<FUNCTION>")) ))
      ((eq? 'external (type-of sexp))
       (tag :i ()
         (text "EXTERN")))))

  (defun render-list-contents (sexp)
    (unless (nil? sexp)
      (render-sexp (head sexp))
      (let ((tail (tail sexp)))
        (if (list? tail)
            (render-list-contents tail)
          (do
            (tag :span '((:class "dot"))
              (text "·"))
            (render-sexp tail))))))

  (defun render-vector-contents (vector i)
    (vector-each vector #'render-sexp))

  (defun style (clauses)
    `(:style ,(css clauses))))


;;;;;

(defmacro defroute (pattern &rest body)
  `(install-route ',pattern (fn (,@(filter pattern #'symbol?))
                              ,(prognify body))))

(defmacro with-authentication (clause &rest body)
  `(authenticate! (fn (,(head clause)) ,@body)))

(defparameter *env* nil)
(defparameter *cwd* ".")

(defun run-command! (&rest cmd)
  (let ((env *env*)
        (cwd *cwd*))
    (print `(run-command! :cwd ,cwd :env ,env :cmd ,cmd))
    (let* ((process (js-call <deno> "run"
                      (js-object "cwd" cwd
                                 "env" (apply #'js-object env)
                                 "cmd" (vector-from-list cmd))))
           (status (await (js-call process "status"))))
      (if (eq? 0 (js-get status "code"))
          t
        (error `(command-error (cwd ,cwd) (cmd ,cmd)))))))

(defun mkdir-recursive! (path)
  (await (js-call  <deno> "mkdir" path (js-object "recursive" t))))

(defun response (status headers &optional body)
  (vector status (apply #'js-object headers) body))

(defvar <speech-recognition> (js-get *window* "webkitSpeechRecognition"))
(defvar <speech-grammar-list> (js-get *window* "webkitSpeechGrammarList"))

(defun listen ()
  (let ((recognition (new <speech-recognition>)))
    (do (js-set! recognition "continuous" nil)
        (js-set! recognition "lang" "en-US")
        (js-set! recognition "interimResults" nil)
        (js-set! recognition "maxAlternatives" 1)
        (await
         (returning
             (new <promise>
                  (callback (resolve)
                    (js-set! recognition "onresult"
                      (callback (event)
                        (log event)
                        (js-call-function resolve
                          (js-get* event '("results" "0" "0" "transcript")))))))
           (js-call recognition "start"))))))

(defun sleep (secs)
  (await
   (new <promise>
        (callback (resolve)
          (js-call *window* "setTimeout" resolve (* 1000 secs))))))

(DEFVAR *GIT* (JS-GET *WINDOW* "git"))
(DEFVAR *GIT-HTTP* (JS-GET *WINDOW* "git_http"))
(DEFVAR *FS* (JS-GET *WINDOW* "fs"))
(DEFVAR *FILESYSTEM* (JS-GET *FS* "promises"))

(defun rmdir-recursive! (path)
  (await (js-call *filesystem* "rm" path (js-object "recursive" t
                                                    "force" t))))

(DEFUN GIT-CLONE (DIR URL REF DEPTH)
  (AWAIT (JS-CALL *FILESYSTEM* "mkdir" DIR))
  (AWAIT (JS-CALL *GIT* "clone"
           (JS-OBJECT "fs" *FS* "http" *GIT-HTTP*
                      "dir" DIR
                      "url" URL
                      "ref" REF
                      "singleBranch" "true"
                      "depth" DEPTH
                      "onProgress" (callback (info)
                                     (log info))))))

(defun git-push! (dir url ref)
  (let ((bearer (string-append "Bearer " (auth0-get-token))))
    (await
     (js-call *git* "push"
       (js-object "fs" *fs*
                  "http" *git-http*
                  "dir" dir
                  "url" url
                  "ref" ref
                  "headers" (js-object "Authorization" bearer))))))

(DEFUN READDIR (DIR) (AWAIT (JS-CALL *FILESYSTEM* "readdir" DIR)))
(DEFUN MKDIR (DIR) (AWAIT (JS-CALL *FILESYSTEM* "mkdir" DIR)))

(defvar *api-url* "http://localhost:8000")

(defun api-url (&rest path)
  (apply #'string-append (cons *api-url* path)))

(DEFUN CLONE-WISP-REPO (DIR)
  (GIT-CLONE DIR "http://localhost:8000/git/~20220419.RSAF5GID6G" "master" 5)
  (READDIR DIR))

(defun git-clone! (key)
  (git-clone (string-append "/" key) (api-url (string-append "/git/" key)) "master" 5)
  (readdir (string-append "/" key)))

(DEFUN FILE-CODE () (JS-CALL *WISP* "domCode" (QUERY-SELECTOR ".file main")))
(DEFUN SAVE-FILE-CODE! (PATH) (AWAIT (JS-CALL *FILESYSTEM* "writeFile" PATH (FILE-CODE) "utf8")))
(defun stat (x) (await (js-call *filesystem* "stat" x)))

(defun git-add! (repo path)
  (js-call *git* "add"
    (js-object "fs" *fs*
               "dir" (string-append "/" repo)
               "filepath" path)))

(defun git-commit! (repo name email message)
  (JS-CALL *GIT* "commit"
    (JS-OBJECT "fs" *FS*
               "dir" (string-append "/" repo)
               "message" message
               "author" (JS-OBJECT "name" name "email" email))))

(defun git-push-wisp! (repo)
  (git-push! (string-append "/" repo)
             (api-url "/git/" repo)
             "master"))

