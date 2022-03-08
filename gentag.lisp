(defpackage #:wisp (:use #:common-lisp))

(defparameter *tags*
  '((int #x00)
    (chr #x11)
    (sys #x12)
    (jet #x13)
    (duo #x15 (car cdr))
    (sym #x16 (str pkg val fun))
    (fun #x17 (env par exp))
    (mac #x18 (env par exp))
    (v32 #x19 (idx len))
    (v08 #x1a (idx len))
    (pkg #x1b (nam sym use))
    (ct0 #x1c (hop env fun arg exp))
    (ct1 #x1d (hop env yay nay))
    (ct2 #x1e (hop env exp))
    (ct3 #x1f (hop env exp dew arg))))

(defun zig-tag-enum ()
  (format t
          "pub const Tag = enum(u5) {~%~
            ~:{    ~(~A~) = 0x~X,~%~}~
           };~%"
          *tags*))

(defun zig-col-enum-fn ()
  (format t "pub fn ColEnum(comptime t: Tag) type {~%")
  (format t "    return switch (t) {~%")
  (dolist (tag *tags*)
    (format t "        .~(~A~) => " (car tag))
    (let ((row (cddr tag)))
      (if row
          (format t "enum { ~{~(~A~)~^, ~} },~%" row)
          (format t "void,~%"))))
  (format t "    };~%}~%"))

(defun zig-pointer-tags ()
  (format t "pub const pointerTags = .{~%")
  (dolist (tag *tags*)
    (when (cddr tag)
      (format t "    .~(~A~),~%" (car tag))))
  (format t "};~%"))

(defun zig ()
  (format t "// This file is generated automatically.~%~%")
  (zig-tag-enum)
  (format t "~%")
  (zig-col-enum-fn)
  (format t "~%")
  (zig-pointer-tags))
