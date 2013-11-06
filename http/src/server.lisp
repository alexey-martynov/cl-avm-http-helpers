(in-package #:cl-avm-http-helpers.server)

(defgeneric http-header (header implementation)
  (:documentation "HTTP-HEADER function provides generic access to
the HTTP protocol request headers.

IMPLEMENTATION parameter is the symbol which indentifies actual implementation.
HEADER is the symbol which identifies header, for example, :IF-MODIFIED-SINCE."))

(defgeneric (setf http-header) (header value implementation)
  (:documentation "(SETF HTTP-HEADER) function provides generic access to
the HTTP protocol response headers.

IMPLEMENTATION parameter is the symbol which indentifies actual implementation.
HEADER is the symbol which identifies header, for example, :LAST-MODIFIED.
VALUE is the value of the header"))

(defgeneric http-status (status-code implementation)
  (:documentation "HTTP-STATUS function converts HTTP status code like 404 to
the proper HTTP server implementation value, like HUNCHENTOOT:+HTTP-NOT-FOUND+"))

#+hunchentoot (defmethod http-header (header (implementation (eql :hunchentoot)))
                (hunchentoot:header-in* header))

#+hunchentoot (defmethod (setf http-header) (value header (implementation (eql :hunchentoot)))
                (setf (hunchentoot:header-out header) value))

#+hunchentoot (defmethod http-status (status (implementation (eql :hunchentoot)))
                status)
