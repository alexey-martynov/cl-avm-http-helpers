(defpackage :cl-avm-http-helpers
  (:use :cl
        :alexandria
        :hunchentoot
        :local-time)
  (:export #:parse-mime-type
           #:mimetype=
           #:parse-accept-header
           ;; Handle Accept header
           #:dispatch-mime-type

           ;; Handle conditionals
           #:when-modified
           #:when-modified*
           #:unless-modified
           #:unless-modified*))
