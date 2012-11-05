(defsystem cl-avm-http-helpers
  :name "cl-avm-http-helpers"
  :author "Alexey Martynov"
  :depends-on (#:alexandria)
  :components ((:file "packages")
               (:module "src"
                        :depends-on ("packages")
                        :components ((:file "tools")
                                     (:file "mime" :depends-on ("tools"))
                                     (:file "accept" :depends-on ("tools" "mime"))))))
