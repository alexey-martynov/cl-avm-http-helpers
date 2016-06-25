(in-package #:cl-avm-http-helpers)

(defun parse-etag-condition-header (header)
  "Parse list of ETags for If-Match, If-None-Match headers and return
list of strings or list of single :ANY keyword"
  (when header
    (let ((str (trim header)))
      (if (string= "*" str)
          :ANY
          (let (result
                value)
            (map 'string (named-lambda process-character (c)
                           (cond
                             ;; For simplicity ignore all garbage between quoted values
                             ((and value (char= #\" c))
                              (if result
                                  (push value result)
                                  (setf result (list value)))
                              (setf value nil))
                             ((char= #\" c)
                              (setf value (make-array 10 :fill-pointer 0 :adjustable t :element-type 'character)))
                             (value
                              (vector-push c value)))
                           #\Space)
                 str)
            (nreverse result))))))

(defmacro if-match* ((etag implementation &optional (etag-matched (intern "ETAG-MATCHED" *package*))) &body body)
  (with-gensyms (handler etag-header)
    (once-only ((object-etag etag)
                (impl implementation))
      `(labels ((,handler (,etag-matched)
                  (declare (ignorable ,etag-matched))
                  ,@body))
         (let ((,etag-header (parse-etag-condition-header (http-header :IF-MATCH ,impl))))
           (cond
             ((and ,etag-header ,object-etag)
              ;; Compare and invoke
              ;; FIXME: remove FORMAT here
              ;; FIXME: :ANY must be a value not part of list
              (if (or (eq :ANY ,etag-header) (member (format nil "\"~A\"" ,object-etag)
                                                     ,etag-header
                                                     ;; FIXME: Properly split quotes
                                                     :test #'(lambda (etag item)
                                                               (or (eq :ANY item)
                                                                   (string= etag item)))))
                  (,handler t)
                  (http-status +http-precondition-failed+ ,impl)))
             ((and ,etag-header (not ,object-etag))
              ;; Not found?
              (if (eq :ANY ,etag-header)
                  ;; Requirement from RFC 2616 (12.24)
                  (http-status +http-precondition-failed+ ,impl)
                  (http-status +http-not-found+ ,impl)))
             ((and (not ,etag-header) ,object-etag)
              ;; Precondition Required because of ETag existence on resource
              (http-status +http-precondition-required+ ,impl))
             (t
              ;; ETag nor If-Match header exists so just invoke
              (,handler nil))))))))

(defmacro if-match ((etag &optional (etag-matched (intern "ETAG-MATCHED" *package*))) &body body)
  `(if-match* (,etag (detect-http-implementation) ,etag-matched)
     ,@body))
