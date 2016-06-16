(defsystem cl-avm-http-helpers+hunchentoot
  :name "cl-avm-http-helpers"
  :author "Alexey Martynov"
  :depends-on (#:hunchentoot #:cl-avm-http-helpers)
  :components ((:module "http"
                        :components ((:module "src"
                                              :components ((:file "hunchentoot")))))))
