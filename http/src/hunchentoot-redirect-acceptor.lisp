(in-package #:cl-avm-http-helpers.hunchentoot)

(defclass redirect-acceptor (acceptor)
  ((target-port :initarg :target-port
                :initform (error "TARGET-PORT is required")
                :type (integer 1 65535)
                :documentation "Target port to redirect to.")
   (target-protocol :initarg :target-protocol
                    :initform :http
                    :type (member :http :https)
                    :documentation "Target protocol for redirection. Should be :HTTP or :HTTPS with default value :HTTPS."))
  (:documentation "Simple redirection acceptor for Hunchentoot. Mostly used to redirect from HTTP URLs to HTTPS."))

(defmethod acceptor-dispatch-request ((redirector redirect-acceptor) request)
  (redirect (request-uri request)
            :port (slot-value redirector 'target-port)
            :protocol (slot-value redirector 'target-protocol)))
