(defsystem cl-avm-http-helpers+hunchentoot
  :name "cl-avm-http-helpers"
  :description "Various HTTP helpers to simplify server-side development. Hunchentoot-bound stuff"
  :author "Alexey Martynov"
  :license "MIT"
  :depends-on (#:hunchentoot #:cl-avm-http-helpers)
  :components ((:module "http"
                :components ((:module "src"
                              :components ((:file "hunchentoot")
                                           (:file "hunchentoot-redirect-acceptor")))))))
