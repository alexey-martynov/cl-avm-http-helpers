(defpackage :cl-avm-http-helpers-tests
  (:use :cl
        :fiveam
        :cl-avm-http-helpers))

(in-package :cl-avm-http-helpers-tests)

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system ':cl-avm-http-helpers-tests))))
  (run! 'accept-header)
  (run! 'conditionals))
