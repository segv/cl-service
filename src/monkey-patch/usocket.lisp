(in-package :usocket)

(defun local-socket-listen (socket-pathname &key (backlog 5) (protocol :stream) (element-type 'character) (abstract-p nil))
  (let ((sock (make-instance (if abstract-p
                                 'sb-bsd-sockets:local-abstract-socket
                                 'sb-bsd-sockets:local-socket)
                             :type protocol)))
    (sb-bsd-sockets:socket-bind sock (namestring socket-pathname))
    (sb-bsd-sockets:socket-listen sock backlog)
    (make-stream-server-socket sock :element-type element-type)))

(defun local-socket-connect (socket-pathname &key (abstract-p nil) (protocol :stream) (element-type 'character))
  (let ((sock (make-instance (if abstract-p
                                 'sb-bsd-sockets:local-abstract-socket
                                 'sb-bsd-sockets:local-socket)
                             :type protocol)))
    (sb-bsd-sockets:socket-connect sock socket-pathname)
    (sb-bsd-sockets:socket-make-stream sock
                                       :input t
                                       :output t
                                       :buffering :full
                                       :element-type element-type
                                       :serve-events nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(local-socket-listen
            local-socket-connect)
          :usocket))
