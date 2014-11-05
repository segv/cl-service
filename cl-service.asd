(in-package :common-lisp-user)

(asdf:defsystem :cl-service
  :depends-on (:chanl
               :cl-fad
               :sb-daemon
               :flexi-streams
               :alexandria
               :sb-posix
               :yason)
  :components ((:module :src
                :serial t
                :components ((:file "packages")
                             (:file "service")
                             (:file "events")
                             (:file "control-server")
                             (:file "signal-handlers")
                             (:file "pid-file-lock")
                             (:file "start")

                             (:file "user-api")
                             ))))
