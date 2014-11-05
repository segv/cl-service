(in-package :cl-service)

(define-condition service-already-running (error)
  ((service :initarg :service)
   (other-pid :initarg :other-pid))
  (:report (lambda (e s)
             (let ((pidfile (pid-pathname-of (slot-value e 'service)))
                   (name    (name-of (slot-value e 'service))))
               (format s "Can not lock pidfile at ~S. ~A is already running (with pid ~A).~%"
                       pidfile
                       name
                       (slot-value e 'other-pid))))))

(defgeneric acquire-pid-file-lock (service)
  (:method ((service service))
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((filename (namestring (ensure-directories-exist (pid-pathname-of service))))
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
            (error e))))
      (sb-posix:ftruncate fd 0)
      (sb-posix:lseek fd 0 sb-posix:seek-set)

      (let* ((string (princ-to-string (sb-posix:getpid)))
             (buffer (flexi-streams:string-to-octets string :external-format :utf-8)))
        (sb-posix:write fd (sb-sys:vector-sap buffer) (length buffer)))

      service)))
