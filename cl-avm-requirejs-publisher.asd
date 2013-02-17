(defsystem cl-avm-requirejs-publisher
    :name "cl-avm-requirejs-publisher"
    :author "Alexey Martynov"
    :depends-on (#:restas #:closure-template #:alexandria)
    :components ((:module "requirejs"
                          :components ((:file "packages")
                                       (:file "publisher" :depends-on ("packages"))))))
