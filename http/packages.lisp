(defpackage :cl-avm-http-helpers.server
  (:use :cl)
  (:export #:http-header
           #:http-status))

(defpackage :cl-avm-http-helpers
  (:use :cl
        :alexandria
        :cl-avm-http-helpers.server)
  (:export #:*default-http-implementation*
           #:parse-mime-type
           #:mimetype=
           #:parse-accept-header
           ;; Handle Accept header
           #:dispatch-mime-type

           ;; Handle conditionals
           #:when-modified
           #:when-modified*
           #:unless-modified
           #:unless-modified*))
