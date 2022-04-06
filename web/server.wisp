(with-simple-error-handler ()
  (async
   (fn ()
     (defun serve (port handler)
       (let ((callback (make-pinned-value
                        (fn (req)
                          (async
                           (fn ()
                             (with-simple-error-handler ()
                               (call handler req))))))))
         (await
          (returning
              (js-call *wisp* "serve" callback
                       (js-object "port" port))
            (print `(http server port ,port))))))

     (defvar <response> (js-get *window* "Response"))

     (defun new (constructor &rest args)
       (apply #'js-new (cons constructor args)))

     (defun response (status headers body)
       (new <response> body
            (js-object "status" status
                       "headers" (apply #'js-object headers))))

     (serve 8000
            (fn (req)
              (progn
                (print `(http request ,(js-get req "url")))
                (response 200 '("content-type" "text/plain")
                  "Hello from Wisp!\n")))))))
