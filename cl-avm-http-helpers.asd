(defsystem cl-avm-http-helpers
  :name "cl-avm-http-helpers"
  :author "Alexey Martynov"
  :depends-on (#:alexandria)
  :in-order-to ((test-op (test-op cl-avm-http-helpers-tests)))
  :components ((:module "http"
                        :components ((:file "packages")
                                     (:module "src"
                                              :depends-on ("packages")
                                              :components ((:file "tools")
                                                           (:file "mime" :depends-on ("tools"))
                                                           (:file "accept" :depends-on ("tools" "mime"))))))))

(defsystem cl-avm-http-helpers-tests
    :name "cl-avm-http-helpers-tests"
    :author "Alexey Martynov"
    :depends-on (#:cl-avm-http-helpers #:fiveam)
    :components ((:module "test"
                          :pathname "http/test"
                          :components ((:file "runner")
                                       (:file "accept-header" :depends-on ("runner"))))))
