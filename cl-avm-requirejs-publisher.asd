(defsystem cl-avm-requirejs-publisher
    :name "cl-avm-requirejs-publisher"
    :author "Alexey Martynov"
    :depends-on (#:restas #:closure-template #:alexandria #:bordeaux-threads #:cl-avm-http-helpers)
    :components ((:module "requirejs"
                          :components ((:file "packages")
                                       (:file "publisher" :depends-on ("packages"))))))
