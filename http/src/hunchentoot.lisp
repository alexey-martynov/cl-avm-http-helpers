(in-package #:cl-avm-http-helpers.server)

#+hunchentoot (defmethod http-header (header (implementation (eql :hunchentoot)))
                (hunchentoot:header-in* header))

#+hunchentoot (defmethod (setf http-header) (value header (implementation (eql :hunchentoot)))
                (setf (hunchentoot:header-out header) value))

#+hunchentoot (defmethod http-status (status (implementation (eql :hunchentoot)))
                status)

#+hunchentoot (defmethod http-request-method ((implementation (eql :hunchentoot)))
                (hunchentoot:request-method*))

#+hunchentoot (defmethod http-is-succeeded (response (implementation (eql :hunchentoot)))
                (not (and (integerp response) (>= response 300))))
