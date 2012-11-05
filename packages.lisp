(defpackage :cl-avm-http-helpers
  (:use :cl
        :alexandria)
  (:export #:parse-mime-type
           #:mimetype=
           #:parse-accept-header
           #:handle-mime-types))
