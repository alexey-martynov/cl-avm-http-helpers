(in-package :cl-avm-http-helpers)

(defun parse-accept-header (header)
  (let ((len (length header)))
    (do ((start 0 (min len (1+ delim)))
         (delim (position-or-end #\, header 0 len) (position-or-end #\, header (min len (1+ delim)) len))
         result)
        ((>= start len) result)
      (let* ((mime-type (parse-mime-type header :start start :end delim :reverse t))
             (qparam (member-if #'(lambda (str)
                                    (let ((pos (position #\= str)))
                                      (and pos (string= "q" (trim (subseq str 0 pos))))))
                                (second mime-type)))
             (q 1.0))
        (when qparam
          (let ((pos (position #\= (car qparam))))
            (setf q (read-from-string (subseq (car qparam) (1+ pos)))
                  (second mime-type) (nreverse (cdr qparam))))
          (unless (typep q 'single-float)
            (error "Invalid quality value")))
        (if-let (item (member (car (first mime-type)) result :key #'car :test #'string-equal))
          (if-let (subitem (member (cdr (first mime-type)) (cdar item) :key #'car :test #'string-equal))
            (setf (cdar subitem) (max (cdar subitem) q))
            (push (cons (cdr (first mime-type)) q) (cdar item)))
          (push `(,(car (first mime-type)) (,(cdr (first mime-type)) . ,q)) result))))))

(defun find-matched-type (type-cons header)
  (let ((type (car type-cons))
        (subtype (cdr type-cons))
        strict-match
        half-pattern-match
        pattern-match)
    (dolist (target header)
      (let ((target-type (car target)))
        (when (or (string= "*" target-type) (string-equal target-type type))
          (dolist (subtarget (cdr target))
            (let ((subtarget-type (car subtarget)))
              (when (or (string= "*" subtarget-type) (string-equal subtarget-type subtype))
                (cond
                  ((string= "*" target-type)
                   (setf pattern-match (cdr subtarget)))
                  ((string= "*" subtarget-type)
                   (setf half-pattern-match (cdr subtarget)))
                  (t
                   (setf strict-match (cdr subtarget))))))))))
    (values strict-match half-pattern-match pattern-match)))

(defmacro handle-mime-types ((header) &body body)
  (with-gensyms (parsed-header
                 handled-types
                 strict half-pattern pattern
                 strict-q half-pattern-q pattern-q
                 item
                 matched-strict matched-half-pattern matched-pattern)
    (let* (on-unhandled
           (handlers (mapcar #'(lambda (item)
                                 (if (eq 'otherwise (car item))
                                     (progn
                                       (setf on-unhandled (cdr item))
                                       nil)
                                     (let ((sym (gensym "mth")))
                                       `(,(car item) ,sym ,(cdr item)))))
                            body)))
      `(let ((,parsed-header (parse-accept-header ,header))
             (,handled-types '(,@(mapcar #'(lambda (item)
                                             (when item
                                               (cons (parse-mime-type (first item)) (second item))))
                                         handlers)))
             ,strict ,half-pattern ,pattern
             (,strict-q -1.0)
             (,half-pattern-q -1.0)
             (,pattern-q -1.0))
         (dolist (,item ,handled-types)
           (when ,item
             (multiple-value-bind (,matched-strict ,matched-half-pattern ,matched-pattern) (find-matched-type (caar ,item) ,parsed-header)
               (when (or ,matched-strict ,matched-half-pattern ,matched-pattern)
                 (when (and ,matched-strict (> ,matched-strict ,strict-q))
                   (setf ,strict-q ,matched-strict
                         ,strict (cdr ,item)))
                 (when (and ,matched-half-pattern (> ,matched-half-pattern ,half-pattern-q))
                   (setf ,half-pattern-q ,matched-half-pattern
                         ,half-pattern (cdr ,item)))
                 (when (and ,matched-pattern (> ,matched-pattern ,pattern-q))
                   (setf ,pattern-q ,matched-pattern
                         ,pattern (cdr ,item)))))))
           ;; TODO: Add context
           (case (or ,strict ,half-pattern ,pattern)
             ,@(mapcar #'(lambda (item)
                           `(,(second item) ,@(third item)))
                       handlers)
             ,(when on-unhandled
               `(t ,@on-unhandled))
             )))))

(defun sample-handlers (header)
  (handle-mime-types (header)
                     ("text/plain"
                      (format t "Plain Text~%"))
                     ("text/jpeg"
                      (format t "JPEG image~%"))
                     ("image/png"
                      (format t "PNG Image~%"))
                     ("text/html"
                      (format t "HTML~%"))
                     (otherwise
                      (format t "Not matched~%"))
                     )
  )
