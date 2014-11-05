(in-package :cl-service)


;;;; The service class.

(defclass service ()
  ((name         :accessor name-of
                 :initform "(unnamed service)"
                 :documentation "A string naming this service. Used only to generate human readable
                 debug/log messages, the value of this slot will only be printed, it will never be
                 used as a key or anything similar.")
   (pid-pathname :accessor pid-pathname-of
                 :documentation "The name, a pathname designator, of the file the service should write
                 its PID to.

                 A lock on this same file will be used to ensure that there is only one service
                 running on a given machine. This means that two services will happily start up if
                 they are using different pid files.")
   (control-socket-pathname :accessor control-socket-pathname-of
                            :documentation "The name, a pathname designator, of the socket file used
                            to send and receive control events.")

   (start-time :accessor start-time-of
               :initform nil)

   (daemonize-p :accessor daemonize-p-of
                :initform nil)

   ;; internal slots.

   (control-server-thread :accessor control-server-thread-of)
   (event-processor-thread :accessor event-processor-thread-of)

   (event-queue :accessor event-queue-of
                :initform (make-instance 'chanl:unbounded-channel))
   (event-queue-thread :accessor event-queue-thread-of)

   (shutdown-event-listeners :accessor shutdown-event-listeners-of
                             :initform '()
                             :documentation "A list of chanl:channels, each one will be chanl:send'd the symbol T when the service shutdown."))
  (:documentation "Class for cl-service services.

This class is effectively a singleton, due to how io redirection, file logging and daemonization work
it really doesn't make any sense to have more than one service instance in a given image. For that
reason the public API never mentions the service instance explicitly, it is always accessed via the
global variable *service*.

For the purposes of adapting cl-service itself to a specific use case the class may be sub classed and
an instance of the given subclassed assigned to the global *service* variable."))

(defmethod (setf run-directory-of) (directory (service service))
  (assert (name-of service)
          (service)
          "Must provide a service name if (setf run-directory-of) is used.")
  (setf (pid-pathname-of service) (path:catfile directory (make-pathname :name (name-of service) :type "pid"))
        (control-socket-pathname-of service) (path:catfile directory (make-pathname :name (name-of service) :type "control"))))


;;;; The global state

(defvar *service* (make-instance 'service)
  "The service object. All public api functions will access the global cl-service via this object.")


;;;; When we daemonizing we lose the repl which causes sbcl to exit as soon as cl-service:start
;;;; returns. Since cl-service runs all its server/event loops in the background cl-service:start
;;;; returns fairly quickly (certainly much much earlier than a stop or kill event is received), these
;;;; two methods provide a wait/notify mechanism so that, when daemonizing, we can delay the return of
;;;; the start function until the service is actually exiting.

(defmethod wait-until-shutdown ((service service))
  "Blocks until SERVICE's shutdown-event has been signaled."
  (let ((channel (make-instance 'chanl:unbounded-channel)))
    (push channel (shutdown-event-listeners-of service))
    (chanl:recv channel)))

(defmethod wake-for-shutdown ((service service))
  "Signals the shutdown-event on SERVICE. All calls to wait-until-shutdown will now return."
  (dolist (channel (shutdown-event-listeners-of service))
    (chanl:send channel t))
  service)


;;;; A trivial wrapper around sb-daemon:daemonize

(defmethod daemonize-process ((service service))
  "Convert the current process into a unix daemon process; see sb-daemon:daemonize for details."
  (sb-daemon:daemonize :pidfile nil
                       :exit-parent t
                       :input t
                       :output t
                       :error t
                       :disable-debugger nil)
  service)


;;;; IO redirection. This is provided mainly as a convenience for users of cl-service who want to
;;;; redirect the standard output stream before daemonizing.

(defmethod redirect-io-using-service ((service service) stream &key streams broadcast-to-source)
  (dolist (stream-name streams)
    (setf (symbol-value stream-name) (if broadcast-to-source
                                         (make-broadcast-stream (symbol-value stream-name) stream)
                                         stream))))
