(in-package :cl-avm-http-helpers)

(defun parse-mime-type (type &key (start 0) end reverse)
  "Parse MIME type string from TYPE starting at position START and finishing
at position END. The result will be in form of a list
((type . subtype) options*)
where every option is a pair of (name . value)"
  (let ((len (length type)))
    (unless end
      (setf end len))
    (let* ((pos (position-or-end #\; type start end))
           (mime-type (let ((p (position-or-end #\/ type start pos)))
                        (cons (trim (subseq type start p))
                              (trim (subseq type (1+ p) pos))))))
      (setf start (min end (1+ pos)))
      (let ((params (do ((delim (position-or-end #\; type start end) (position-or-end #\; type start end))
                         result)
                        ((>= start end) (if reverse result (nreverse result)))
                      (push (trim (subseq type start delim)) result)
                      (setf start (min end (1+ delim))))))
        (list mime-type (mapcar #'(lambda (item)
                                       (let ((delim (position-or-end #\= item 0 (length item))))
                                         (cons (trim (subseq item 0 delim))
                                               (trim (subseq item (+ delim 1))))))
                                   params))))))

(defun format-mime-type (type)
  "Format result of parsing back to string"
  (with-output-to-string (result)
    (format result "~A/~A" (car (first type)) (cdr (first type)))
    (mapc #'(lambda (item)
              (format result ";~A=~A" (car item) (cdr item)))
          (second type))))

(defun mimetype= (lhs rhs)
  "Compare two parsed MIME type strings for equality.
The MIME type string should be parsed as the following list:
'((type . subtype) (parameters))'. Pattern (*) substitution is performed but
parameters are not compared.
Second return value shows\"strength\" of equality:
0 - matched to */*
1 - matched to type/*
2 - exact match"
  ;; Destructure elements
  (let ((lhs-type (car (first lhs)))
        (rhs-type (car (first rhs)))
        (lhs-subtype (cdr (first lhs)))
        (rhs-subtype (cdr (first rhs))))
    ;; Quick check: if type is "*" then subtype must be "*"
    (when (and (string= lhs-type "*") (string/= lhs-subtype "*"))
      (error "Invalid MIME type pattern"))
    (when (and (string= rhs-type "*") (string/= rhs-subtype "*"))
      (error "Invalid MIME type pattern"))
    (cond
      ((or (string= lhs-type "*") (string= rhs-type "*"))
       ;; One of types is "*/*" which match any type
       (values t 0))
      ((string-equal lhs-type rhs-type)
       ;; Compare subtypes
       (if (or (string= lhs-subtype "*") (string= rhs-subtype "*"))
           (values t 1)
           (if (string-equal lhs-subtype rhs-subtype) (values t 2) nil)))
      (t
       nil))))
