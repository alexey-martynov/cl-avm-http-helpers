(in-package #:cl-avm-http-helpers-tests)

(defmethod cl-avm-http-helpers.server:http-header (header (implementation (eql :test)))
  (declare (ignore header)))

(defmethod (setf cl-avm-http-helpers.server:http-header) (value header (implementation (eql :test)))
  (declare (ignore value header)))

(defmethod cl-avm-http-helpers.server:http-status (status (implementation (eql :test)))
  status)

(defmethod cl-avm-http-helpers.server:http-is-succeeded (response (implementation (eql :test)))
  (not (and (integerp response) (>= response 300))))
