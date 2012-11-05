(in-package :cl-avm-http-helpers)

(defun trim (str)
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed) str))

(defun position-or-end (elem seq start end)
  (or (position elem seq :start start :end end) end))
