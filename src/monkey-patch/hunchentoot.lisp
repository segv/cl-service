(in-package :hunchentoot)

;;;; Make the function hunchentoot:acceptor-make-request a generic function.
;;;;
;;;; This allow our local-socket-acceptor to provide another implementation of the function which
;;;; fakes the address data (a domain socket doesn't really have a remote host, let alone a remote
;;;; port)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'acceptor-make-request))

(defgeneric acceptor-make-request (acceptor socket &key headers-in
                                                        content-stream
                                                        method
                                                        uri
                                                        server-protocol)
  (:documentation "Make a REQUEST instance for the ACCEPTOR, setting up those slots that are
determined from the SOCKET by calling the appropriate socket query functions." ))

(defmethod acceptor-make-request ((acceptor acceptor) socket &key headers-in
                                                                  content-stream
                                                                  method
                                                                  uri
                                                                  server-protocol)
  (multiple-value-bind (remote-addr remote-port)
      (get-peer-address-and-port socket)
    (multiple-value-bind (local-addr local-port)
        (get-local-address-and-port socket)
      (make-instance (acceptor-request-class acceptor)
                     :acceptor acceptor
                     :local-addr local-addr
                     :local-port local-port
                     :remote-addr remote-addr
                     :remote-port remote-port
                     :headers-in headers-in
                     :content-stream content-stream
                     :method method
                     :uri uri
                     :server-protocol server-protocol))))
