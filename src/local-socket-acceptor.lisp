(in-package :cl-service)

(defclass local-socket-acceptor (hunchentoot:acceptor)
  ((socket-filename :initarg :socket-filename :accessor socket-filename))
  (:documentation "A hunchentoot acceptor which listens on a local domain socket."))

(defmethod print-object ((acceptor local-socket-acceptor) stream)
  (print-unreadable-object (acceptor stream :type t)
    (write-string (namestring (socket-filename acceptor)) stream)))

(defmethod hunchentoot:acceptor-address ((acceptor local-socket-acceptor))
  (namestring (socket-filename acceptor)))

(defmethod hunchentoot:acceptor-port ((acceptor local-socket-acceptor))
  0)

(defmethod shared-initialize :after ((acceptor local-socket-acceptor) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (when (and (slot-boundp acceptor 'socket-filename)
             (socket-filename acceptor))
    ;; this just forces us to call the (setf method) which has the "convert to filename pathname" logic.
    (setf (socket-filename acceptor) (socket-filename acceptor))))

(defmethod (setf socket-filename) (pathname (acceptor local-socket-acceptor))
  (setf (slot-value acceptor 'socket-filename) (cl-fad:pathname-as-file pathname)))

(defmethod hunchentoot:start-listening ((acceptor local-socket-acceptor))
  (with-accessors ((listen-socket hunchentoot::acceptor-listen-socket))
      acceptor
    (when listen-socket
      (hunchentoot:hunchentoot-error "acceptor ~A is already listening" acceptor))
    (setf (hunchentoot::acceptor-listen-socket acceptor)
          (usocket:local-socket-listen (socket-filename acceptor)
                                       :backlog (hunchentoot:acceptor-listen-backlog acceptor)
                                       :element-type '(unsigned-byte 8)))
    (values)))

(defmethod hunchentoot::acceptor-make-request ((acceptor local-socket-acceptor) socket &key headers-in
                                                                                            content-stream
                                                                                            method
                                                                                            uri
                                                                                            server-protocol)
  (make-instance (hunchentoot:acceptor-request-class acceptor)
                 :acceptor acceptor
                 :local-addr (namestring (socket-filename acceptor))
                 :local-port 0
                 :remote-addr (namestring (socket-filename acceptor))
                 :remote-port 0
                 :headers-in headers-in
                 :content-stream content-stream
                 :method method
                 :uri uri
                 :server-protocol server-protocol))
