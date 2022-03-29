(defun mock-idom! ()
  (defun idom-open-start! (tag-name)
    (write "<")
    (write tag-name)
    (write " "))

  (defun idom-attr! (attr value)
    (write attr)
    (write "='")
    (write value)
    (write "' "))

  (defun idom-open-end! ()
    (write ">"))

  (defun idom-text! (text)
    (write text))

  (defun idom-close! (tag-name)
    (write "</")
    (write tag-name)
    (write ">")))
