(in-package :cl-service)

(defun send-command (socket-filename command &rest arguments)
  (let* ((stream (flexi-streams:make-flexi-stream (chunga:make-chunked-stream
                                                   (usocket:local-socket-connect (namestring socket-filename)
                                                                                 :element-type '(unsigned-byte 8)))
                                                  :external-format (flexi-streams:make-external-format :latin-1
                                                                                                       :eol-style :lf))))
    (unwind-protect
         (let ((drakma:*header-stream* nil))
           (multiple-value-bind (body status)
               (drakma:http-request (make-instance 'puri:uri
                                                   :scheme :http
                                                   :host "localhost"
                                                   :path (concatenate 'string "/" command))
                                    :parameters (loop for (key value) on arguments by #'cddr
                                                      collect (cons (etypecase key
                                                                      (keyword (string-downcase (symbol-name key)))
                                                                      (string key))
                                                                    (princ-to-string value)))
                                    :protocol :http/1.0
                                    :method :GET
                                    :stream stream
                                    :force-binary t
                                    :external-format-out :utf-8)


             (unless (= 200 status)
               (format *error-output* "~S returned status code ~S." command status)
               (sb-ext:exit :code 1))

             (when body
               (write-string (flexi-streams:octets-to-string body :external-format :utf-8) *standard-output*))

             (sb-ext:exit :code 0)))
      (close stream))))
