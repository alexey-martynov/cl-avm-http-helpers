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

;(defun parse-rfc850-timestamp (str)
;  ;; Example:
;  ;; Sunday, 06-Nov-94 08:49:37 GMT
;  ;; This format contains fixed-length, fixed-position field
;  ;; AFTER week day. So we need to find a base and use it in offets
;  (if-let (base (position #\Space str))
;    (let *
;
;  0)

(defun parse-http-timestamp (str)
  (if (> (length str) 3)
      ;; String has enough length to check it's type
      (case (elt str 3)
        (#\,
         (parse-rfc1123-timestamp str))
                                        ;        (otherwise
                                        ;         (parse-rfc850-timestamp str)))
        )      -2))

(defmacro when-modified* (date implementation &body body)
  (once-only ((dt date)
              (impl implementation))
    (with-gensyms (response)
      `(if (< (parse-http-timestamp (http-header :IF-MODIFIED-SINCE ,impl)) ,dt)
             (let ((,response (progn
                                ,@body)))
               (when (http-is-succeeded ,response ,impl)
                 (setf (http-header :LAST-MODIFIED ,impl) (format-http-date ,dt)))
               ,response)
         (http-status +http-not-modified+ ,impl)))))

(defmacro when-modified (date &body body)
  `(when-modified* ,date (detect-http-implementation)
     ,@body))

(defmacro unless-modified* (date implementation &body body)
  (once-only ((dt date)
              (impl implementation))
    `(if (<= ,dt (parse-http-timestamp (http-header :IF-UNMODIFIED-SINCE ,impl)))
         (progn
           ,@body)
         (http-status +http-precondition-failed+ ,impl))))

(defmacro unless-modified (date &body body)
  `(unless-modified* ,date (detect-http-implementation)
     ,@body))
