(in-package :cl-service)

;;;; Events are the actual things that we tell a service to do. Events can be triggered either by
;;;; calling trigger-event directly, by sending a request to the control server, or by sending a
;;;; signal to the process.

(defvar *event-table* '())

(defmethod event-table-of ((service service))
  *event-table*)

(defmethod (setf event-table-of) (table (service service))
  (setf *event-table* table))

(defun (setf on-event) (handler event-name)
  (setf (on-event-of *service* event-name) handler))

(defmethod (setf on-event-of) (handler (service service) event-name)
  (setf (assoc-value (event-table-of service) event-name :test #'string=) handler))

(defun no-op ()
  t)

(defun standard-reload-event ()
  (trigger-event *service* "load"))

(defun standard-status-event ()
  (list "pid" (sb-posix:getpid)
        "control-socket" (namestring (control-socket-pathname-of *service*))
        "uptime" (- (get-universal-time) (start-time-of *service*))))

(define-condition swank-package-not-found (error)
  ())

(defun standard-create-swank-server-event (&key (port nil port-p))
  (if (find-package :swank)
      (let ((create-swank-server (read-from-string "swank:create-swank-server")))
        (list "port" (if port-p
                         (funcall create-swank-server :port port)
                         (funcall create-swank-server))))
      (error 'swank-package-not-found)))

(defun standard-eval-event (&key form package forms)
  (dolist (form (append (list form) forms))
    (let ((*package* (find-package (string-upcase package))))
      (eval (read-from-string form)))))

(setf (on-event "load")  'no-op
      (on-event "start") 'no-op
      (on-event "stop")  'no-op
      (on-event "kill")  'no-op
      (on-event "reload") 'standard-reload-event
      (on-event "status") 'standard-status-event
      (on-event "create-swank-server") 'standard-create-swank-server-event
      (on-event "eval") 'standard-eval-event)

(defmethod trigger-event ((service service) event-name &key arguments (wait-p t))
  (if wait-p
      (let ((response-channel (make-instance 'chanl:unbounded-channel)))
        (chanl:send (event-queue-of service)
                    (list event-name arguments response-channel))
        (chanl:recv response-channel))
      (progn
        (chanl:send (event-queue-of service)
                    (list event-name arguments nil))
        nil)))

(defmethod find-event-handler ((service service) event-name &key (error-p t))
  (let ((event (assoc event-name (event-table-of service) :test #'string=)))
    (when error-p
      (assert (and event (cdr event))
              (event-name service)
              "Unable to find handler for event named ~S in ~S's event-table." event-name service))
    (values (cdr event)
            (if event
                t
                nil))))

(defclass event-error ()
  ((event-name :initarg :event-name :reader event-name-of)
   (arguments  :initarg :arguments :reader arguments-of)
   (error      :initarg :error :reader error-of)
   (backtrace  :initarg :backtrace :reader backtrace-of)))

(defgeneric event-error-p (object)
  (:method ((object event-error)) t)
  (:method ((object t)) nil))

(defmethod process-event ((service service) event-name arguments response-channel)
  (let* ((value (block nil
                  (handler-bind ((error (lambda (e)
                                          (return (make-instance 'event-error
                                                                 :error e
                                                                 :event-name event-name
                                                                 :arguments arguments
                                                                 :backtrace (with-output-to-string (*debug-io*)
                                                                              (sb-debug:print-backtrace)))))))
                    (let ((*service* service))
                      (apply (find-event-handler service event-name) arguments))))))

    (when response-channel
      (chanl:send response-channel value))

    (when (member event-name '("stop" "kill") :test #'string=)
      (sb-ext:exit :code 0 :abort (cond
                                    ((string= event-name "stop")
                                     nil)
                                    ((string= event-name "kill")
                                     t))))

    value))

(defmethod start-event-processor ((service service))
  (setf (event-processor-thread-of service)
        (sb-thread:make-thread (lambda () (run-event-processor service))
                               :name (format nil "Event processing loop for ~A" (name-of service)))))

(defmethod run-event-processor ((service service))
  (loop
    with shutdown-channel = (make-instance 'chanl:unbounded-channel)
    initially (push shutdown-channel (shutdown-event-listeners-of service))
    do (chanl:select
        ((chanl:recv shutdown-channel _)
         (return-from run-event-processor _))
        ((chanl:recv (event-queue-of service) event)
         (process-event service
                        (first event)
                        (second event)
                        (third event)))
        (else
         (sleep 0.1)))))
