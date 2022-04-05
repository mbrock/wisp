(NOTE (:COPYRIGHT) (SPDX-LICENSE-IDENTIFIER "AGPL-3.0-or-later") (AUTHOR "mikael@brockman.se") (PROJECT "https://wisp.town"))
(NOTE (:DEMO) (AUTH0-LOGIN) (AUTH0-GET-TOKEN) (TOWN-REQUEST "/foo") (TOWN-CREATE-REPO))
(DEFVAR *WINDOW* (JS-GLOBAL-THIS))
(DEFVAR *GIT* (JS-GET *WINDOW* "git"))
(DEFVAR *GIT-HTTP* (JS-GET *WINDOW* "git_http"))
(DEFVAR *FS* (JS-GET *WINDOW* "fs"))
(DEFVAR *PFS* (JS-GET *FS* "promises"))
(DEFVAR *DOCUMENT* (JS-GET *WINDOW* "document"))
(DEFVAR *CONSOLE* (JS-GET *WINDOW* "console"))
(DEFUN LOG (X) (JS-CALL *CONSOLE* "log" X))
(DEFUN JSON (STR) (JS-CALL (JS-GET *WINDOW* "JSON") "parse" STR))
(DEFUN THEN (P F) (JS-CALL P "then" (MAKE-PINNED-VALUE F)))
(DEFUN CATCH (P F) (JS-CALL P "catch" (MAKE-PINNED-VALUE F)))
(DEFUN ASYNC (F) (CALL-WITH-PROMPT :ASYNC F (FN (V K) (THEN V (FN (X) (ASYNC (FN () (CALL K X))))))))
(DEFUN AWAIT (X) (SEND! :ASYNC X))
(DEFMACRO FETCH (URL &REST OPTS) `(AWAIT (JS-CALL *WINDOW* "fetch" ,URL (JS-OBJECT ,@OPTS))))
(DEFUN RESPONSE-TEXT (X) (AWAIT (JS-CALL X "text")))
(DEFUN GITHUB-CLONE (DIR URL REF DEPTH) (AWAIT (JS-CALL *PFS* "mkdir" DIR)) (AWAIT (JS-CALL *GIT* "clone" (JS-OBJECT "fs" *FS* "http" *GIT-HTTP* "dir" DIR "corsProxy" "https://cors.node.town" "url" URL "ref" REF "singleBranch" "true" "depth" DEPTH))))
(DEFUN READDIR (DIR) (AWAIT (JS-CALL *PFS* "readdir" DIR)))
(DEFUN MKDIR (DIR) (AWAIT (JS-CALL *PFS* "mkdir" DIR)))
(DEFUN CLONE-WISP-REPO (DIR) (GITHUB-CLONE DIR "https://github.com/mbrock/wisp" "master" 5) (READDIR DIR))
(DEFUN FILE-CODE () (JS-CALL *WISP* "domCode" (QUERY-SELECTOR "#file")))
(DEFUN SAVE-FILE-CODE (PATH) (AWAIT (JS-CALL *PFS* "writeFile" PATH (FILE-CODE) "utf8")))
 
(DEFVAR *WISP-REPO-DIR* "/wisp6")
(DEFUN GIT-EXAMPLE () (ASYNC (FN () (OUTPUT (CLONE-WISP-REPO "/wisp6")) (SAVE-FILE-CODE "/wisp6/web/src/user.wisp") (LOG (AWAIT (JS-CALL *GIT* "add" (JS-OBJECT "fs" *FS* "dir" "/wisp6" "filepath" "web/src/user.wisp")))) (PRINT (JS-CALL *GIT* "commit" (JS-OBJECT "fs" *FS* "dir" "/wisp6" "message" "First commit from Wisp" "author" (JS-OBJECT "name" "Mikael Brockman" "email" "mikael@brockman.se")))))))
(DEFUN ALERT (X) (JS-CALL *WINDOW* "alert" X))
(DEFUN RANDOM-DICE () (READ-MANY-FROM-STRING (RESPONSE-TEXT (FETCH "https://www.random.org/integers/?num=10&min=1&max=6&col=1&base=10&format=plain&rnd=new"))))
(DEFUN $ (SELECTOR) (JS-CALL *DOCUMENT* "querySelector" SELECTOR))
(NOTE (:APRIL 4 2022) (TODO GIT COMMIT) (TODO GIT PUSH))
(NOTE (:APRIL 1 2022) (DONE JS FFI) (TODO KEY :C TO CHANGE) (TODO PREVENT CURSOR SHENANIGANS) (DONE KEY (CTRL :E) TO EVAL TOP-LEVEL FORM) (TODO AUTOCOMPLETE ALL SYMBOLS) (TODO HANDLE EVALUATION ERRORS) (TODO REFACTOR WASD INTERFACE) (TODO CSS AS SEXP) (DONE READ MANY INSERTED FORMS) (DONE WHOLE DEXP NAVIGATION BY DEFAULT) (DONE LOCALSTORAGE SAVING))
(NOTE (:MARCH 31 2022) (DONE IMPLEMENT INSERTING IN STRUCTURAL EDITOR) (DONE STRUCTURE TO CODE STRING) (DONE EVALUATING EXPRESSIONS) (TODO SAVING FILE))
(NOTE (:MARCH 29 2002) (DONE IMPLEMENT STRUCTURAL EDITOR) (DONE PLAY AROUND) (TODO BUY BANANAS))
