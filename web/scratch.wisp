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

(DEFVAR *GIT* (JS-GET *WINDOW* "git"))
(DEFVAR *GIT-HTTP* (JS-GET *WINDOW* "git_http"))
(DEFVAR *FS* (JS-GET *WINDOW* "fs"))
(DEFVAR *FILESYSTEM* (JS-GET *FS* "promises"))

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
(DEFUN read-file-code! (path)
  (await (js-call *filesystem* "readFile" PATH "utf8")))

(defun stat (x) (await (js-call *filesystem* "stat" x)))

(defun git-add! (repo path)
  (js-call *git* "add"
    (js-object "fs" *fs*
               "dir" repo
               "filepath" path)))

(defun git-init! (repo)
  (await
   (js-call *git* "init"
     (js-object "fs" *fs* "dir" (string-append "/" repo)))))

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
