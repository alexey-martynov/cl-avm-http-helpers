(in-package #:cl-avm-http-helpers)

(defparameter +rfc1123-month+ '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun parse-rfc1123-timestamp (str)
  ;; Example:
  ;; Sun, 06 Nov 1994 08:49:37 GMT
  ;; RFC-1123 Date format is fixed-length, fixed-position
  ;; Use these fixed places
  (let ((day (parse-integer str :start 5 :end 7 :junk-allowed t :radix 10))
        (year (parse-integer str :start 12 :end 16 :junk-allowed t :radix 10))
        (hour (parse-integer str :start 17 :end 19 :junk-allowed t :radix 10))
        (min (parse-integer str :start 20 :end 22 :junk-allowed t :radix 10))
        (sec (parse-integer str :start 23 :end 25 :junk-allowed t :radix 10)))
    (if (and day year hour min sec
             ;; Sanity check - timezone exists and "GMT"
             (<= 29 (length str))
             (string= "GMT" (subseq str 26 29)))
        ;; Convert month
        (if-let (month (position (subseq str 8 11) +rfc1123-month+ :test #'string=))
          (encode-universal-time sec min hour day (1+ month) year 0)
          0)
        0)))

(defun parse-rfc850-timestamp (str)
  ;; Example:
  ;; Sunday, 06-Nov-94 08:49:37 GMT
  ;; This format contains fixed-length, fixed-position field
  ;; AFTER week day. So we need to find a base and use it in offets
  (if-let (base (position #\Space str))
    (let *

  0)

(defun parse-http-timestamp (str)
  (if (> (length str) 3)
      ;; String has enough length to check it's type
      (case (elt str 3)
        (#\,
         (parse-rfc1123-timestamp str))
        (otherwise
         (parse-rfc850-timestamp str)))
      -2))

(defmacro when-modified* (date header &body body)
  (once-only ((dt date)
              (hdr header))
    `(if (< (parse-http-timestamp ,hdr) ,dt)
         (progn
           (setf (hunchentoot:header-out :LAST-MODIFIED) (rfc-1123-date ,dt))
           ,@body)
         hunchentoot:+http-not-modified+)))

(defmacro when-modified (date &body body)
  (when-modified* date (hunchentoot:header-in* :IF-MODIFIED-SINCE)
    body))

(defmacro unless-modified* (date header &body body)
  (once-only ((dt date)
              (hdr header))
    `(if (<= ,dt (parse-http-timestamp ,hdr))
         (progn
           ,@body)
         hunchentoot:+http-precondition-failed+)))

(defmacro unless-modified (date &body body)
  (unless-modified* date (hunchentoot:header-in* :IF-UNMODIFIED-SINCE)
    body))
