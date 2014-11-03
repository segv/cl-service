(asdf:defsystem :cl-service
  :depends-on (:sb-daemon :flexi-streams :cl-fad :alexandria :sb-posix :hunchentoot :yason :drakma)
  :components ((:module :src
                :components ((:module :monkey-patch
                              :components ((:file "usocket")
                                           (:file "hunchentoot")))
                             (:file "packages")
                             (:file "local-socket-acceptor" :depends-on (:monkey-patch
                                                                         "packages"))
                             (:file "cl-service" :depends-on (:monkey-patch
                                                              "packages"
                                                              "local-socket-acceptor"))
                             (:file "client" :depends-on (:monkey-patch
                                                          "packages"))))))
