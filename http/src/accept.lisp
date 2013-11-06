(in-package :cl-avm-http-helpers)

(defstruct mime-paramset params (qvalue 1.0 :type single-float))

(defstruct mime-subtype name params)

(defstruct mime-type name subtypes)

(defun parse-accept-header (header)
  (let ((len (length header)))
    (do ((start 0 (min len (1+ delim)))
         (delim (position-or-end #\, header 0 len) (position-or-end #\, header (min len (1+ delim)) len))
         result)
        ((>= start len) result)
      (let* ((mime-type (parse-mime-type header :start start :end delim :reverse t))
             (qparam (member "q" (second mime-type) :key #'car :test #'string-equal))
             (params (sort (if qparam (cdr qparam) (second mime-type))
                           #'string<
                           :key #'car))
             (q 1.0))
        (when qparam
          (setf q (read-from-string (cdar qparam)))
          (unless (typep q 'single-float)
            (error "Invalid quality value")))
        (if-let (item (member (car (first mime-type)) result :key #'mime-type-name :test #'string-equal))
          (if-let (subitem (member (cdr (first mime-type)) (mime-type-subtypes (first item)) :key #'mime-subtype-name :test #'string-equal))
            (if-let (param-item (member params (mime-subtype-params (first subitem)) :key #'mime-paramset-params :test #'equal))
              (setf (mime-paramset-qvalue (first param-item)) (max (mime-paramset-qvalue (first param-item)) q))
              (push (make-mime-paramset :params params :qvalue q) (mime-subtype-params (first subitem))))
            (push (make-mime-subtype :name (cdr (first mime-type))
                                     :params (list (make-mime-paramset :params params
                                                                       :qvalue q)))
                  (mime-type-subtypes (first item))))
          (push (make-mime-type :name (car (first mime-type))
                                :subtypes (list (make-mime-subtype :name (cdr (first mime-type))
                                                                   :params (list (make-mime-paramset :params params
                                                                                                     :qvalue q)))))
                result))))))

(defun find-qvalue (type subtype params header)
  (when-let (matched-type (find type header :key #'mime-type-name :test #'string-equal))
    (when-let (matched-subtype (find subtype (mime-type-subtypes matched-type) :key #'mime-subtype-name :test #'string-equal))
      (when-let (matched-params (find params (mime-subtype-params matched-subtype) :key #'mime-paramset-params :test #'equal))
        (mime-paramset-qvalue matched-params)))))

(defun find-qvalue* (type subtype params header)
  (if-let (q (find-qvalue type subtype params header))
    q
    (find-qvalue type subtype nil header)))

(defmacro dispatch-mime-type** (header &body body)
  (let* (on-unhandled
         (handlers (nreverse (reduce #'(lambda (result item)
                                         (if (eq 'otherwise (car item))
                                             (progn
                                               (setf on-unhandled (cdr item))
                                               result)
                                             (let ((type (parse-mime-type (car item))))
                                               (when (or (string= "*" (car (first type)))
                                                         (string= "*" (cdr (first type))))
                                                 (error "MIME type \"~A\" contains pattern which is forbidden in cases." (car item)))
                                               (when (find "q" (second type) :key #'car :test #'string=)
                                                 (error "MIME type \"~A\" parameter contains q-value which is forbidden in cases." (car item)))
                                               (cons (list type (gensym) (cdr item)) result))))
                                     body
                                     :initial-value '()))))
    (with-gensyms (type-handlers parsed-header handler)
      `(let ((,type-handlers ',(mapcar #'(lambda (item)
                                           `(,(first item) ,(second item)))
                                       handlers))
             (,parsed-header (parse-accept-header ,header)))
         (let ((,handler (reduce (named-lambda find-matched-type! (result item)
                                   (let* ((current-q (car result))
                                          (type (car (first (first item))))
                                          (subtype (cdr (first (first item))))
                                          (params (sort (second (first item))
                                                        #'string<
                                                        :key #'car)))
                                     (let ((q (or (find-qvalue* type subtype params ,parsed-header)
                                                  (find-qvalue* type "*" params ,parsed-header)
                                                  (find-qvalue* "*" "*" params ,parsed-header))))
                                       (if (and q (< current-q q))
                                           (cons q (second item))
                                           result))))
                                 ,type-handlers
                                 :initial-value '(0.0 . nil))))
           (case (cdr ,handler)
             ,@(mapcar #'(lambda (item)
                           `(,(second item) ,@(third item)))
                       handlers)
             ,@(when on-unhandled
                     `((t ,@on-unhandled)))
             ))))))

(defmacro dispatch-mime-type* (implementation &body body)
  (once-only ((impl implementation))
    `(progn
       (setf (http-header :VARY ,impl) "Accept")
       (dispatch-mime-type** (http-header :ACCEPT ,impl)
         ,@body))))

(defmacro dispatch-mime-type (&body body)
  `(dispatch-mime-type* (detect-http-implementation)
     ,@body))
