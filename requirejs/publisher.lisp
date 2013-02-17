(in-package #:cl-avm-requirejs-publisher)

(defparameter *source-dir* nil)
(defparameter *cache-dir* nil)

(defparameter *extension* "tmpl")

(defun make-source-name (path)
  (when *source-dir*
    (let ((name (pathname-name path))
          (ext (pathname-type path)))
      (when (string= "js" ext)
        (let ((path-name (merge-pathnames (make-pathname :directory (pathname-directory path)
                                                         :name name
                                                         :type *extension*)
                                          *source-dir*)))
          (probe-file path-name))))))

(restas:define-route route ("*path" :method :GET)
  (:content-type "text/javascript")
  (if-let (source-file (make-source-name (pathname (format nil "~{~A~^/~}" path))))
    (closure-template:compile-template :requirejs-backend source-file)
    hunchentoot:+http-not-found+))
