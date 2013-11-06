(in-package :cl-avm-http-helpers)

(defparameter *default-http-implementation* nil "Default HTTP implementation to use")

(defparameter +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))


(defun detect-http-implementation ()
  (or *default-http-implementation*
      (find :hunchentoot *features*)))

(defun format-http-date (timestamp)
  "Format the TIMESTAMP as string according to RFC 1123. The TIMESTAMP should be a unversal time"
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time timestamp 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(defun trim (str)
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed) str))

(defun position-or-end (elem seq start end)
  (or (position elem seq :start start :end end) end))
