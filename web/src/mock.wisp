(defun mock-dom! ()
  (defun dom-open-start! (tag-name)
    (write "<")
    (write tag-name)
    (write " "))

  (defun dom-attr! (attr value)
    (write attr)
    (write "='")
    (write value)
    (write "' "))

  (defun dom-open-end! ()
    (write ">"))

  (defun dom-text! (text)
    (write text))

  (defun dom-close! (tag-name)
    (write "</")
    (write tag-name)
    (write ">")))
