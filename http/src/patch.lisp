(in-package :cl-avm-http-helpers)

(define-condition invalid-action-specifier ()
  ((specifier :initarg :specifier))
  (:report (lambda (condition stream)
             (format stream "Invalid action specifier ~S." (slot-value condition 'specifier)))))

(define-condition action-handler-not-found ()
  ((action-name :initarg :action-name)
   (handlers :initarg :handlers))
  (:report (lambda (condition stream)
             (format stream "Action ~S is not found in list ~{ ~S~#[~; and ~:;,~]~}."
                     (slot-value condition 'action-name)
                     (slot-value condition 'handlers)))))

(defmacro handle-patch-actions ((request &optional object) &body handlers)
  "Build PATCH method handler by apply actions sequentially. REQUEST is the list
of the alists with action information. Each action should contain value with key
:ACTION and value of type string which identifies actual action. HANDLERS is the
list of action handlers. Each action handler should have a form (NAME FUNCTION)
where NAME is the string with action name and FUNCTION are actual handler. The
FUNCTION will be called with 2 parameters: the first is the object to update and
the second is the alist of action. This handler should return updated object as
the result. This updated will be passed to the next action. The result of the
HANDLE-PATCH-ACTIONS is the result of the last action."
  (with-gensyms (handler-map)
    `(let ((,handler-map (list ,@(mapcar #'(lambda (item) `(cons ,(first item) ,(second item))) handlers))))
       (reduce (named-lambda handle-action! (acc action)
                 (restart-case (let ((name (assoc :action action)))
                                 (unless name
                                   (error 'invalid-action-specifier :action action))
                                 (if-let ((action-function (assoc (cdr name) ,handler-map :test #'string=)))
                                   (funcall (cdr action-function) acc (rest action))
                                   (progn
                                     (error 'action-handler-not-found :action (cdr name) :handlers (mapcar #'car ,handler-map)))))
                   (ignore-action () acc)
                   (use-value (value) value)))
               ,request
               :initial-value ,object))))


(handle-patch-actions ('() nil)
  ("action1" #'(lambda (a b) (format t "action1 ~S ~S~%" a b) 1))
  ("action2" #'(lambda (a b) (format t "action2 ~S ~S~%" a b) 2)))
