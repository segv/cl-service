(in-package :common-lisp-user)

(defpackage :cl-service
  (:use :common-lisp :alexandria)
  (:export
   ;;;; user api
   #:name
   #:run-directory
   #:pid-pathname
   #:control-socket-pathname
   #:start

   #:on-event

   #:on-load
   #:on-start
   #:on-stop
   #:on-kill
   #:on-reload
   #:on-status

   #:redirect-io))
