(in-package :cl-avm-http-helpers-tests)

(def-suite accept-header)

(in-suite accept-header)

(defun select-mime-type (header)
  (handle-mime-types (header)
    ("text/plain"
     "text/plain")
    ("text/jpeg"
     "text/jpeg")
    ("image/png"
     "image/png")
    ("text/html"
     "text/html")
    (otherwise
     nil)))

(test without-qvalue
  (is (string= "text/plain" (select-mime-type "text/plain")))
  (is (string= "text/plain" (select-mime-type "text/plain, text/html")))
  (is (string= "text/plain" (select-mime-type "image/png,text/plain,text/html")))
  (is (string= "image/png" (select-mime-type "image/png,text/html")))
  (is (not (select-mime-type "image/gif"))))

(test with-qvalue
  (is (string= "text/plain" (select-mime-type "text/plain;q=0.9")))
  (is (string= "text/plain" (select-mime-type "text/plain,text/html;q=0.8")))
  (is (string= "text/plain" (select-mime-type "text/plain;q=0.9,text/html;q=0.8")))
  (is (string= "text/plain" (select-mime-type "image/png;q=0.3,text/plain;q=0.9,text/html;q=0.5"))))

(test subtype-range-without-qvalue
  (is (string= "text/plain" (select-mime-type "text/*")))
  (is (string= "text/plain" (select-mime-type "text/plain, text/*")))
  (is (string= "text/plain" (select-mime-type "image/*,text/plain,text/*")))
  (is (string= "image/png" (select-mime-type "image/png,text/*"))))

(test type-range-without-qvalue
  (is (string= "text/plain" (select-mime-type "*/*")))
  (is (string= "image/png" (select-mime-type "image/png,*/*"))))
