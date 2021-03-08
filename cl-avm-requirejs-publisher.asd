(defsystem cl-avm-requirejs-publisher
  :name "cl-avm-requirejs-publisher"
  :description "RESTAS module to publish CL-CLOSURE-TEMPLATE templates as RequireJS modules."
  :author "Alexey Martynov"
  :license "MIT"
  :depends-on (#:restas #:closure-template #:alexandria #:bordeaux-threads #:cl-avm-http-helpers)
  :components ((:module "requirejs"
                :components ((:file "packages")
                             (:file "publisher" :depends-on ("packages"))))))
