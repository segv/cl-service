(in-package :cl-service)

(defmethod start-service ((service service) &key daemonize with-signal-handlers with-control-server)
  (setf (daemonize-p-of service) daemonize)

  (funcall (find-event-handler service "load"))

  (when daemonize
    (daemonize-process service))

  (handler-case
      (acquire-pid-file-lock service)
    (service-already-running (e)
      (format *error-output* "~A" e)
      ;; EX_UNAVAILABLE
      (sb-ext:exit :code 69)))

  (start-event-processor service)

  (trigger-event service "start")

  (setf (start-time-of service) (get-universal-time))

  (when with-control-server
    (start-control-server service))

  (when with-signal-handlers
    (install-signal-handlers service))

  (when daemonize
    (wait-until-shutdown service))

  service)
