(defpackage :cl-avm-http-helpers.http
  (:use :cl)
  (:export #:+http-not-modified+
           #:+http-precondition-failed+
           #:+http-not-acceptable+))

(defpackage :cl-avm-http-helpers.server
  (:use :cl)
  (:export #:http-header
           #:http-status
           #:http-is-succeeded))

(defpackage :cl-avm-http-helpers
  (:use :cl
        :alexandria
        :cl-avm-http-helpers.http
        :cl-avm-http-helpers.server)
  (:export #:*default-http-implementation*
           #:parse-mime-type
           #:mimetype=
           #:parse-accept-header
           ;; Handle Accept header
           #:dispatch-mime-type
           #:dispatch-mime-type*

           ;; Handle conditionals
           #:when-modified
           #:when-modified*
           #:unless-modified
           #:unless-modified*))
