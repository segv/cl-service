(asdf:defsystem :cl-service
  :depends-on (:sb-daemon :flexi-streams :cl-fad :alexandria :sb-posix :hunchentoot :yason)
  :components ((:module :src
                :serial t
                :components ((:module :monkey-patch
                              :components ((:file "usocket")
                                           (:file "hunchentoot")))
                             (:file "packages")
                             (:file "util")
                             (:file "local-socket-acceptor")
                             (:file "cl-service")))))
