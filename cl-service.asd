(asdf:defsystem :cl-service
  :depends-on (:chanl
               :sb-daemon
               :flexi-streams
               :alexandria
               :sb-posix
               :yason)
  :components ((:module :src
                :components ((:file "packages")
                             (:file "cl-service" :depends-on ("packages"))))))
