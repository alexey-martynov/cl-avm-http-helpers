(restas:define-module #:cl-avm-requirejs-publisher
  (:use :cl
        :closure-template
        :alexandria
        :cl-avm-http-helpers)
  (:export #:*source-dir*
           #:*cache-dir*))
