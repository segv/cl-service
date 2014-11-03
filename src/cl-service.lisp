(in-package :cl-service)


;;;; The core SERVICE class definition.

(defclass service ()
  ((name         :reader name
                 :initarg :name
                 :initform "(unnamed service)"
                 :documentation "A string naming this service. Used only to generate human readable
                 debug/log messages, the value of this slot will only be printed, it will never be
                 used as a key or anything similar.")
   (pid-filename :reader pid-filename
                 :initarg :pid-filename
                 :documentation "The name, a pathname designator, of the file the service should write
                 its PID to.

                 A lock on this same file will be used to ensure that there is only one service
                 running on a given machine. This means that two services will happily start up if
                 they are using different pid files.")
   (control-socket-filename :reader control-socket-filename
                            :initarg :control-socket-filename
                            :documentation "The name, a pathname designator, of the socket file used
                            to send and receive control commands.")
   (error-handler :reader error-handler
                  :initarg :error-handler
                  :initform 'log-and-die
                  :documentation "A function which will be called, in an error handler established via
                  handler-bind, when a service related method signals, and does not handle, a
                  condition of type error.

                  Can be set to one of cl-service:log-and-die, cl-service:just-die,
                  cl-service:abort-method, or, obviously, some just handler. If the slot's value is
                  NIL no handler will be installed and the implementation specific behaviour will
                  apply.")

   ;; internal slots. note that none of these have an :initarg.

   (control-acceptor :reader control-acceptor
                     :initform nil)
   (log-stream   :accessor log-stream
                 :initform nil)))


;;;; standard event handlers.

(defgeneric on-start (service)
  (:method ((service service))
    t))

(defgeneric on-stop (service)
  (:method ((service service))
    t))

(defgeneric on-kill (service)
  (:method ((service service))
    t))

(defgeneric on-status (service)
  (:method ((service service))
    (list :status "up"
          :pid (parse-integer (alexandria:read-file-into-string (pid-filename service))))))


;;;; error handling "framework"

(defmacro with-service-error-handler ((service) &body body)
  `(call-with-service-error-handler service (lambda () ,@body)))

(defmethod call-with-service-error-handler ((service service) thunk)
  (catch 'abort
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (when (error-handler service)
                              (funcall (error-handler service))))))
      (funcall thunk))))

(defun log-and-die ()
  (when (find-restart 'log-and-die)
    (invoke-restart 'log-and-die)))

(defun just-die ()
  (when (find-restart 'just-die)
    (invoke-restart 'just-die)))

(defun abort-method ()
  (throw 'abort nil))


;;;; service life cycle methods

(defgeneric start (service &key daemonize)
  (:documentation "Called to start up SERVICE.")

  (:method ((service-name symbol) &rest start-args)
    (apply #'start (make-instance service-name) start-args))

  (:method  ((service service) &key (daemonize t))
    (with-service-error-handler (service)
      ;; daemonize.(when service.(daemonize-process))
      ;; ==
      ;; (when daemonize service.(daemonize-process))
      (when daemonize
        (daemonize-process service))
      (acquire-pid-file-lock service)
      (redirect-io-streams service daemonize)

      (on-start service)

      ;; service.(create-control-acceptor daemonize).(start-control-acceptor)
      (start-control-acceptor (create-control-acceptor service daemonize)))))

(defmethod daemonize-process ((service service))
  (sb-daemon:daemonize :pidfile nil
                       :exit-parent t
                       :input t
                       :output t
                       :error t
                       :disable-debugger nil))

(defmethod redirect-io-streams ((service service) daemonize)
  (flet ((redirected-stream-for (base-stream)
           (if daemonize
               (log-stream service)
               (make-broadcast-stream (log-stream service) base-stream))))
    (setf (log-stream service) (open-log-stream service)
          *standard-output* (redirected-stream-for *standard-output*)
          *error-output* (redirected-stream-for *error-output*)
          *trace-output* (redirected-stream-for *trace-output*))))

(defmethod create-control-acceptor ((service service) daemonize)
  (setf (slot-value service 'control-acceptor)
        (make-instance (if daemonize
                           'blocking-control-acceptor
                           'background-control-acceptor)
                       :service service
                       :socket-filename (ensure-directories-exist (control-socket-filename service)))))

(defgeneric open-log-stream (service))

(defgeneric close-log-stream (service)
  (:method ((service service))
    (when (and (streamp (log-stream service))
               (open-stream-p (log-stream service)))
      (close (log-stream service)))))

(define-condition service-already-running (error)
  ((service :initarg :service))
  (:report (lambda (e s)
             (let ((pidfile (pid-filename (slot-value e 'service)))
                   (name    (name (slot-value e 'service))))
               (format s "Can not lock pidfile at ~S. ~A is already running (with pid ~A).~%"
                       pidfile
                       name
                       (alexandria:read-file-into-string pidfile :external-format :utf-8))))))

(defgeneric acquire-pid-file-lock (service)
  (:method ((service service))
    (let* ((filename (namestring (ensure-directories-exist (pid-filename service))))
           (fd (sb-posix:open filename (logior sb-posix:o-creat sb-posix:o-wronly) #o644)))
      (handler-case
          (sb-posix:lockf fd sb-posix:f-tlock 0)
        (sb-posix:syscall-error ()
          (let ((e (make-condition 'service-already-running :service service)))
            (restart-case
                (error e)
              (log-and-die ()
                :report "Log error to *error-output* and die."
                (format *error-output* "~A" e)
                ;; according to sysexits.h 75 is EX_TEMPFAIL
                (sb-ext:exit :code 75))
              (just-die ()
                :report "Kill the current process."
                (sb-ext:exit :code 75 :abort nil))))))
      (sb-posix:ftruncate fd 0)
      (sb-posix:lseek fd 0 sb-posix:seek-set)
      (binwrite fd "~D" (sb-posix:getpid)))))


;;;; the control http server

(defclass control-acceptor (local-socket-acceptor hunchentoot:acceptor)
  ((service :reader service :initarg :service)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor control-acceptor) (request hunchentoot:request))
  (let ((service (service acceptor)))
    (alexandria:switch ((hunchentoot:script-name request) :test #'string=)
      ("/stop"
       (sb-thread:make-thread (lambda ()
                                (on-stop service)
                                (hunchentoot:stop acceptor :soft t)
                                (close-log-stream service)
                                (sb-ext:exit :code 0 :abort nil)))
       (response-json :status "ok"))
      ("/status"
       (apply #'response-json (on-status service)))
      ("/kill"
       (let* ((hard (hunchentoot:parameter "hard" request))
              (hard (cond
                      ((null hard)
                       nil)
                      ((and (stringp hard)
                            (member hard '("false" "no")))
                       nil)
                      (t
                       t))))
         (if hard
             (sb-ext:exit :code 2 :abort t)
             (progn
               (sb-thread:make-thread (lambda ()
                                        (on-kill service)
                                        (hunchentoot:stop acceptor :soft nil)
                                        (sb-ext:exit :code 2 :abort t)))
               (response-json :status "ok")))))
      (t
       (setf (hunchentoot:return-code*) 404)
       (json :code "command-not-found"
             :message (format nil "No command name ~S found." (hunchentoot:script-name request)))))))

(defgeneric start-control-acceptor (acceptor))

(defmethod start-control-acceptor :before ((acceptor control-acceptor))
  (when (probe-file (control-socket-filename (service acceptor)))
    (delete-file (control-socket-filename (service acceptor)))))

(defclass background-control-acceptor (control-acceptor)
  ()
  (:default-initargs :taskmaster (make-instance 'hunchentoot:one-thread-per-connection-taskmaster)))

(defmethod start-control-acceptor ((acceptor background-control-acceptor))
  (hunchentoot:start acceptor)
  (format *standard-output* "Started Control Acceptor.~%"))

(defclass blocking-control-acceptor (control-acceptor)
  ()
  (:default-initargs :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)))

(defmethod start-control-acceptor ((acceptor blocking-control-acceptor))
  (format *trace-output* "Starting Control Acceptor.~%")
  ;; since we're running a single threaded taskmaster, this call does not return.
  (hunchentoot:start acceptor))
