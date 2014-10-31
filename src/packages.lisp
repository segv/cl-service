(in-package :common-lisp-user)

(defpackage :cl-service
  (:use :common-lisp)
  (:export #:service
           #:open-log-stream
           #:start
           #:stop

           #:on-start
           #:on-stop
           #:on-kill
           #:on-reload

           #:service-already-running
           #:log-and-die
           #:just-die
           #:do-nothing
           #:abort-method))
