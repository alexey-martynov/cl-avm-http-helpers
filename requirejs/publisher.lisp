(in-package #:cl-avm-requirejs-publisher)

(defparameter *source-dir* nil)
(defparameter *cache-dir* nil)

(defparameter *extension* "tmpl")

(defparameter *cache-lock* nil)
(defparameter *cache-item-locks* nil)
(defparameter *cache-item-spare-locks* nil)

(defmacro with-cache-lock ((path) &body body)
  (with-gensyms (item-lock)
    (once-only (path)
      `(let ((,item-lock (with-lock-held (*cache-lock*)
                           (let ((element (gethash ,path *cache-item-locks*)))
                             (if element
                                 (incf (cdr element))
                                 (let ((lock (if *cache-item-spare-locks*
                                                 (pop *cache-item-spare-locks*)
                                               (make-lock "RequireJS cache item lock"))))
                                   (setf element (cons lock 1))
                                   (setf (gethash ,path *cache-item-locks*) element)))
                             element))))
         (unwind-protect (with-lock-held ((car ,item-lock))
                           ,@body)
           (with-lock-held (*cache-lock*)
             (when (= 0 (decf (cdr ,item-lock)))
               (push (car ,item-lock) *cache-item-spare-locks*)
               (remhash ,path *cache-item-locks*))))))))

(defmethod restas:initialize-module-instance :before ((module (eql #.*package*)) context)
  (restas:with-context context
    (when *cache-dir*
      (restas:context-add-variable context '*cache-lock* (make-lock (with-output-to-string (s)
                                                                      (format s "RequireJS cache lock ~A" *cache-dir*))))
      (restas:context-add-variable context '*cache-item-locks* (make-hash-table :test #'equal)))))

(defun check-source-file (path)
  (when *source-dir*
    (let ((name (pathname-name path))
          (ext (pathname-type path)))
      (when (string= "js" ext)
        (let* ((path-name (merge-pathnames (make-pathname :directory (pathname-directory path)
                                                         :name name
                                                         :type *extension*)
                                          *source-dir*))
               (result (probe-file path-name)))
          (when result
            (values result (file-write-date result))))))))

(defun update-cached-file (relative-name source timestamp)
  (let ((cached-file (merge-pathnames relative-name *cache-dir*)))
    (handler-bind ((error (lambda (e)
                            #+syslog (syslog:format-message :error "reading cached file \"~A\" failed: ~A" cached-file e)
                            #-syslog (declare (ignore e))
                            hunchentoot:+http-internal-server-error+)))
      (with-cache-lock (cached-file)
        (ensure-directories-exist (directory-namestring cached-file))
        (if (and (probe-file cached-file) (<= timestamp (file-write-date cached-file)))
            (open cached-file :element-type 'unsigned-byte)
            (progn
              (handler-bind ((error (lambda (e)
                                      #+syslog (syslog:format-message :error "regenerating cached file \"~A\" from the source \"~A\" failed: ~A" cached-file source e)
                                      #-syslog (declare (ignore e))
                                      (delete-file cached-file)
                                      hunchentoot:+http-internal-server-error+)))
                (with-open-file (output cached-file :direction :output :if-exists :supersede)
                  (write-string (closure-template:compile-template :requirejs-backend source) output))
                (open cached-file :element-type 'unsigned-byte))))))))

(defun get-cached-file (relative-name source timestamp)
  (if *cache-dir*
      (update-cached-file relative-name source timestamp)
      (closure-template:compile-template :requirejs-backend source)))

(restas:define-route route ("*path" :method :GET)
  (:content-type "text/javascript")
  (let ((relative-name (pathname (format nil "~{~A~^/~}" path))))
    (multiple-value-bind (source timestamp) (check-source-file relative-name)
      (if source
          (when-modified timestamp
            #+syslog (syslog:format-message :debug "Reading cached file \"~A\"" relative-name)
            (get-cached-file relative-name source timestamp))
          hunchentoot:+http-not-found+))))
