(in-package #:cl-avm-http-helpers.server)

(defgeneric http-header (header implementation)
  (:documentation "HTTP-HEADER function provides generic access to
the HTTP protocol request headers.

IMPLEMENTATION parameter is the symbol which identifies actual implementation.
HEADER is the symbol which identifies header, for example, :IF-MODIFIED-SINCE."))

(defgeneric (setf http-header) (value header implementation)
  (:documentation "(SETF HTTP-HEADER) function provides generic access to
the HTTP protocol response headers.

IMPLEMENTATION parameter is the symbol which identifies actual implementation.
HEADER is the symbol which identifies header, for example, :LAST-MODIFIED.
VALUE is the value of the header"))

(defgeneric http-status (status-code implementation)
  (:documentation "HTTP-STATUS function converts HTTP status code like 404 to
the proper HTTP server implementation value, like HUNCHENTOOT:+HTTP-NOT-FOUND+"))

(defgeneric http-request-method (implementation)
  (:documentation "HTTP-REQUEST-METHOD provides generic access to request object and
returns current request method as keyword, i.e. :GET or :PUT"))


(defgeneric http-is-succeeded (response implementation)
  (:documentation "HTTP-IS-SUCCEEDED function checks that RESPONSE is succeeded.
For example, with Hunchentoot it is not integer with value greater or equal to 300"))
