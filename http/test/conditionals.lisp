(in-package :cl-avm-http-helpers-tests)

(def-suite conditionals :in http-helpers)

(in-suite conditionals)

(test rfc-1123
  (is (= (encode-universal-time 6 22 19 25 1 2002 0) (cl-avm-http-helpers::parse-http-timestamp "Fri, 25 Jan 2002 19:22:06 GMT")))
  ;; Timezone MUST be GMT
  (is (= 0 (cl-avm-http-helpers::parse-http-timestamp "Fri, 25 Jan 2002 19:22:06 MSK"))))

