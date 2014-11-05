(in-package :cl-service)

(defmethod start-control-server ((service service))
  (let ((control-socket-pathname (ensure-directories-exist (control-socket-pathname-of service))))
    (when (probe-file control-socket-pathname)
      (delete-file control-socket-pathname))
    (setf (control-server-thread-of service)
          (sb-thread:make-thread (lambda ()
                                   (run-control-server service))
                                 :name "CL-SERVICE control server")))
  service)

(defun run-control-server (service)
  (loop
    with socket-filename = (namestring (control-socket-pathname-of service))
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
                (let ((request (yason:parse text-stream
                                            :object-as :hash-table
                                            :json-arrays-as-vectors t
                                            :json-booleans-as-symbols nil
                                            :json-nulls-as-keyword t
                                            :object-key-fn (lambda (key)
                                                             (with-standard-io-syntax
                                                               (let* ((*read-eval* nil)
                                                                      (key-symbol (read-from-string key)))
                                                                 (assert (symbolp key-symbol)
                                                                         (key))
                                                                 (intern (symbol-name key-symbol) :keyword)))))))
                  (multiple-value-bind (output done-p)
                      (process-control-request service request)
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

(defpackage :cl-service.empty-package (:use))

(defmethod process-control-request ((service service) request)
  (assert (<= 1 (length request) 2) (request))
  (let ((event-name (aref request 0))
        (arguments (if (= 2 (length request))
                       (hash-table-plist (aref request 1))
                       nil)))
    (assert (stringp event-name)
            (request))
    (assert (or (hash-table-p arguments)
                (null arguments)))
    (labels ((ret (&rest keys-and-values)
               (return-from process-control-request (plist-hash-table keys-and-values)))
             (ok (&rest keys-and-values)
               (ret "ok" (plist-hash-table keys-and-values)))
             (err (&rest keys-and-values)
               (ret "error" (plist-hash-table keys-and-values))))
      (unless (find-event-handler service event-name)
        (err "code" "event-not-found"
             "message" (format nil "Event named ~S not found." event-name)))
      (switch (event-name :test #'string=)
        ("stop"
         (trigger-event service "stop" :arguments arguments :wait-p nil)
         (ok "result" "stop-initiated"))
        ("kill"
         (trigger-event service "kill" :arguments arguments :wait-p nil)
         (ok "result" "kill-initiated"))
        (t
         (labels ((to-string (object)
                    (let ((*package* (find-package :cl-service.empty-package)))
                      (format nil "~S" object)))
                  (object-class-name (object)
                    (to-string (class-name (class-of object)))
                    ))
           (handler-case
               (let ((value (trigger-event service event-name :arguments arguments :wait-p t)))
                 (if (event-error-p value)
                     (err "code" "event-error"
                          "type" (object-class-name (error-of value))
                          "event" (event-name-of value)
                          "arguments" (to-string (arguments-of value))
                          "error" (to-string (error-of value))
                          "backtrace" (to-string (backtrace-of value)))
                     (apply #'ok value)))
             (error (e)
               (err "code" code
                    "type" (object-class-name e)
                    "exception" (handler-case
                                    (format nil "~S" e)
                                  (error () "#<Nested error>")))))))))))
