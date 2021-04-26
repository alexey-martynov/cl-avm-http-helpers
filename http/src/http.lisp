(in-package :cl-avm-http-helpers)

(defparameter +http-created+               201)
(defparameter +http-no-content+            204)
(defparameter +http-partial-content+       206)

(defparameter +http-not-modified+          304)

(defparameter +http-bad-request+           400)
(defparameter +http-unauthorized+          401)
(defparameter +http-forbidden+             403)
(defparameter +http-not-found+             404)
(defparameter +http-method-not-allowed+    405)
(defparameter +http-not-acceptable+        406)
(defparameter +http-conflict+              409)
(defparameter +http-precondition-failed+   412)
(defparameter +http-unprocessable-entity+  422)
(defparameter +http-precondition-required+ 428)

(defparameter +http-internal-server-error+ 500)
(defparameter +http-service-unavailable+   503)
(defparameter +http-gateway-timeout+       504)
