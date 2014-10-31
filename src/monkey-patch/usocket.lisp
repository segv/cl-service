(in-package :usocket)

(defun local-socket-listen (socket-pathname &key (backlog 5) (element-type 'character) (abstract-p nil))
  (let ((sock (make-instance (if abstract-p
                                 'sb-bsd-sockets:local-abstract-socket
                                 'sb-bsd-sockets:local-socket)
                             :type :stream)))
    (sb-bsd-sockets:socket-bind sock (namestring socket-pathname))
    (sb-bsd-sockets:socket-listen sock backlog)
    (make-stream-server-socket sock :element-type element-type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'local-socket-listen :usocket))
