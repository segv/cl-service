(in-package :cl-service)

(defun (setf name) (name)
  (setf (name-of *service*) name))

(defun (setf pid-pathname) (pathname)
  (setf (pid-pathname-of *service*) (cl-fad:pathname-as-file pathname)))

(defun (setf control-socket-pathname) (pathname)
  (setf (control-socket-pathname-of *service*) (cl-fad:pathname-as-file pathname)))

(defun (setf run-directory) (directory-pathname)
  (setf (run-directory-of *service*) directory-pathname))

(defun redirect-io (new-io-stream &key (streams nil streams-p)
                                       (broadcast-to-source nil broadcast-to-source-p))
  (redirect-io-using-service *service* new-io-stream
                             :streams (if streams-p
                                          streams
                                          '(*standard-output* *error-output* *trace-output*))
                             :broadcast-to-source (if broadcast-to-source-p
                                                      broadcast-to-source
                                                      (not (daemonize-p-of *service*)))))

(defun start (&key (service *service*)
                   (daemonize t)
                   (with-signal-handlers t)
                   (with-control-server t))
  (start-service service
                 :daemonize daemonize
                 :with-signal-handlers with-signal-handlers
                 :with-control-server with-control-server))

(defun eval-code-like-thing (code-like-thing)
  (etypecase code-like-thing
    (cons (eval code-like-thing))
    (function (funcall code-like-thing))
    (string
     (with-input-from-string (code-stream code-like-thing)
       (loop
         with values = nil
         for form = (read code-stream nil code-stream nil)
         until (eq form code-stream)
         do (setf values (multiple-value-list (eval form)))
         finally (return values))))))

(defmacro def-event-setter (macro-name event-name)
  `(defmacro ,macro-name (&body body)
     `(let ((package *package*)
            (readtable *readtable*))
        (setf (on-event ,',event-name)
              (lambda (&rest *arguments*)
                (declare (special *arguments*))
                (let ((*package* package)
                      (*readtable* readtable))
                  ,@(mapcar (lambda (code-like-thing)
                              `(eval-code-like-thing ',code-like-thing))
                            body)))))))

(def-event-setter on-load "load")
(def-event-setter on-start "start")
(def-event-setter on-stop "stop")
(def-event-setter on-kill "kill")
(def-event-setter on-reload "reload")
(def-event-setter on-status "status")
