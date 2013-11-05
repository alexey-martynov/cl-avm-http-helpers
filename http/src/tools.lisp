(in-package :cl-avm-http-helpers)

(defparameter *default-http-implementation* nil "Default HTTP implementation to use")

(defun detect-http-implementation ()
  (or *default-http-implementation*
      (find :hunchentoot *features*)))

(defun trim (str)
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed) str))

(defun position-or-end (elem seq start end)
  (or (position elem seq :start start :end end) end))
