(in-package :cl-avm-http-helpers-tests)

(def-suite accept-header)

(in-suite accept-header)

(defun select-mime-type (header)
  (cl-avm-http-helpers::dispatch-mime-type** header
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

(defun select-mime-type* (header)
  (cl-avm-http-helpers::dispatch-mime-type** header
    ("text/plain"
     "text/plain")
    ("text/html;level=2"
     "text/html;level=2")
    ("text/html"
     "text/html")
    ("text/html;level=1"
     "text/html;level=1")
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
  (is (string= "text/html" (select-mime-type "text/plain;q=0.8,text/html;q=0.9")))
  (is (string= "image/png" (select-mime-type "image/png;q=0.95,text/plain;q=0.9,text/html;q=0.5"))))

(test subtype-range-without-qvalue
  (is (string= "text/plain" (select-mime-type "text/*")))
  (is (string= "text/plain" (select-mime-type "text/plain, text/*")))
  (is (string= "text/plain" (select-mime-type "image/*,text/plain,text/*")))
  (is (string= "text/plain" (select-mime-type "image/png,text/*"))))

(test type-range-without-qvalue
  (is (string= "text/plain" (select-mime-type "*/*")))
  (is (string= "text/plain" (select-mime-type "image/png,*/*"))))

(test rfc-2616-data
  (is (string= "text/plain" (select-mime-type "text/*, text/html, text/html;level=1, */*"))))

(test type-range-with-parameters
  (is (string= "text/html;level=1" (select-mime-type* "text/html;level=1,*/*;q=0.1")))
  (is (string= "text/html;level=2" (select-mime-type* "text/html;level=0.7,text/html;q=0.8,*/*;q=0.1"))))
