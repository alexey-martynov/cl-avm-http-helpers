(defpackage :cl-avm-http-helpers.server
  (:use :cl)
  (:export #:http-header
           #:http-status
           #:http-request-method
           #:http-is-succeeded))

(defpackage :cl-avm-http-helpers
  (:use :cl
        :alexandria
        :cl-avm-http-helpers.server)
  (:export #:+http-no-content+
           #:+http-not-modified+
           #:+http-not-found+
           #:+http-not-acceptable+
           #:+http-precondition-failed+
           #:+http-unprocessable-entity+
           #:+http-precondition-required+
           #:+http-internal-server-error+

           #:*default-http-implementation*
           #:parse-mime-type
           #:format-mime-type
           #:mimetype=
           #:parse-accept-header

           ;; Handle Accept header
           #:dispatch-mime-type
           #:dispatch-mime-type*

           ;; Handle conditionals
           #:when-modified
           #:when-modified*
           #:unless-modified
           #:unless-modified*

           ;; ETag-based conditionals
           #:if-match
           #:if-match*
           #:if-none-match
           #:if-none-match*))
