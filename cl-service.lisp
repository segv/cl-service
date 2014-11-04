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

   (control-server-thread :accessor control-server-thread)
   (signal-queue :accessor signal-queue
                 :initform (make-instance 'chanl:unbounded-channel))
   (signal-handler-thread :accessor signal-handler-thread)
   (log-stream   :accessor log-stream
                 :initform nil)
   (pid :accessor pid
        :initform nil
        :documentation "PID of the parent process.")))


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
    (list "status" "up"
          "pid" (pid service)
          "control-socket" (namestring (control-socket-filename service)))))

(defgeneric on-reload (service)
  (:method ((service service))
    t))


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

(defgeneric start (service &key daemonize install-signal-handlers)
  (:documentation "Called to start up SERVICE.")

  (:method ((service-name symbol) &rest start-args)
    (apply #'start (make-instance service-name) start-args))

  (:method  ((service service) &key (daemonize t) (install-signal-handlers t))
    (with-service-error-handler (service)
      (when daemonize
        (daemonize-process service))
      (setf (pid service) (sb-posix:getpid))
      (acquire-pid-file-lock service)
      (redirect-io-streams service daemonize)

      (on-start service)

      (start-control-server service)
      (when install-signal-handlers
        (install-signal-handlers service)))))

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

(defmethod start-control-server ((service service))
  (let ((control-socket-pathname (ensure-directories-exist (control-socket-filename service))))
    (when (probe-file control-socket-pathname)
      (delete-file control-socket-pathname))
    (setf (control-server-thread service)
          (sb-thread:make-thread (lambda ()
                                   (run-json-server (namestring control-socket-pathname)
                                                    (lambda (command)
                                                      (process-json-command service command))))
                                 :name "CL-SERVICE control server")))
  service)

(defmethod process-json-command ((service service) command)
  (assert (= 1 (length command)) (command))
  (alexandria:switch ((aref command 0) :test #'string=)
    ("status"
     (alexandria:plist-hash-table (on-status service)))
    ("stop"
     (sb-thread:make-thread (lambda ()
                              (on-stop service)
                              (chanl:send (signal-queue service) :quit)
                              (sb-ext:exit :code 0 :abort nil)))
     (values (alexandria:plist-hash-table (list "status" "shutting-down")) t))
    ("kill"
     (sb-ext:exit :code 0 :abort t))
    ("reload"
     (on-reload service)
     (alexandria:plist-hash-table (list "status" "ok")))
    ("create-swank-server"
     (alexandria:plist-hash-table (list "status" "ok")))
    (t
     (alexandria:plist-hash-table (list "status" "unknown-command" "command-name" (aref command 0))))))

(defmethod install-signal-handlers ((service service))
  (setf (signal-handler-thread service)
        (sb-thread:make-thread
         (lambda ()
           (loop
             for signal = (chanl:recv (signal-queue service))
             ;; write this command to the control socket
             do (when (eql :quit signal)
                  (return nil))
             do (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
                  (sb-bsd-sockets:socket-connect socket (namestring (control-socket-filename service)))
                  (unwind-protect
                       (write-string (ecase signal
                                       ((#.sb-posix:sigabrt #.sb-posix:sigterm)
                                        "[\"stop\"]")
                                       (#.sb-posix:sigint
                                        "[\"kill\"]")
                                       (#.sb-posix:sighup
                                        "[\"reload\"]"))
                                     (flexi-streams:make-flexi-stream
                                      (sb-bsd-sockets:socket-make-stream socket :input t
                                                                                :output t
                                                                                :buffering :full
                                                                                :element-type '(unsigned-byte 8))
                                      :external-format :utf-8))
                    (sb-bsd-sockets:socket-close socket)))))
         :name "CL-SERVICE signal handling loop"))
  (flet ((enable (signo)
           (sb-sys:enable-interrupt signo
                                    (lambda (signo context info)
                                      (declare (ignore context info))
                                      (chanl:send (signal-queue service) signo)))))
    (enable sb-posix:sigterm)
    (enable sb-posix:sigabrt)
    (enable sb-posix:sighup)
    (enable sb-posix:sigint)))

(defgeneric open-log-stream (service))

(defgeneric close-log-stream (service)
  (:method ((service service))
    (when (and (streamp (log-stream service))
               (open-stream-p (log-stream service)))
      (close (log-stream service)))))

(define-condition service-already-running (error)
  ((service :initarg :service)
   (other-pid :initarg :other-pid))
  (:report (lambda (e s)
             (let ((pidfile (pid-filename (slot-value e 'service)))
                   (name    (name (slot-value e 'service))))
               (format s "Can not lock pidfile at ~S. ~A is already running (with pid ~A).~%"
                       pidfile
                       name
                       (slot-value e 'other-pid))))))

(defgeneric acquire-pid-file-lock (service)
  (:method ((service service))
    (let* ((filename (namestring (ensure-directories-exist (pid-filename service))))
           (fd (sb-posix:open filename (logior sb-posix:o-creat sb-posix:o-wronly) #o644))
           (flock (make-instance 'sb-posix:flock :type sb-posix:f-wrlck
                                                 :whence sb-posix:seek-set
                                                 :start 0
                                                 :len 0)))
      (handler-case
          (sb-posix:fcntl fd sb-posix:f-setlk flock)
        (sb-posix:syscall-error ()
          (let ((e (make-condition 'service-already-running :service service)))
            (sb-posix:fcntl fd sb-posix:f-getlk flock)
            (setf (slot-value e 'other-pid) (sb-posix:flock-pid flock))
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

      (let* ((string (princ-to-string (sb-posix:getpid)))
             (buffer (flexi-streams:string-to-octets string :external-format :utf-8)))
        (sb-posix:write fd (sb-sys:vector-sap buffer) (length buffer)))

      service)))


;;;; The super simple control server

(defun run-json-server (socket-filename handler)
  (loop
    with server-socket = (make-instance 'sb-bsd-sockets:local-socket :type :stream)
    initially (sb-bsd-sockets:socket-bind server-socket socket-filename)
    initially (sb-bsd-sockets:socket-listen server-socket 256)
    with shutdown = nil
    until shutdown
    for socket = (sb-bsd-sockets:socket-accept server-socket)
    for binary-stream = (sb-bsd-sockets:socket-make-stream socket
                                                           :input t
                                                           :output t
                                                           :buffering :full
                                                           :timeout 1
                                                           :auto-close t
                                                           :element-type '(unsigned-byte 8))
    for text-stream = (flexi-streams:make-flexi-stream binary-stream :external-format :utf-8)
    do (unwind-protect
            (handler-case
                (let ((input (yason:parse text-stream
                                          :object-as :hash-table
                                          :json-arrays-as-vectors t
                                          :json-booleans-as-symbols nil
                                          :json-nulls-as-keyword t
                                          :object-key-fn 'identity)))
                  (multiple-value-bind (output done-p)
                      (funcall handler input)
                    (setf shutdown done-p)
                    (yason:encode output text-stream)))
              (error (e)
                (handler-case
                    (yason:with-output (text-stream)
                      (yason:with-object ()
                        (yason:encode-object-element "status" "error")
                        (yason:encode-object-element "error" (princ-to-string e))))
                  (error (nested-e)
                    (ignore-errors
                     (format *error-output* "Nested errors in run-json-server: ~S,~S" e nested-e))))))
         (ignore-errors
          ;; the stream may or may not still be open, the client may or may not still be
          ;; functional. as far as the json server is concerned it doesn't matter if this call
          ;; succeeds or not, as long as we don't leak file descriptors we're really ok.
          (close text-stream)))))
