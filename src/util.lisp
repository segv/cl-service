(in-package :cl-service)

(defun binwrite (fd control &rest arguments)
  (let* ((buffer (flexi-streams:string-to-octets (apply #'format nil control arguments) :external-format :utf-8)))
    (sb-posix:write fd (sb-sys:vector-sap buffer) (length buffer))))

(defun hash (&rest keys-and-values)
  (alexandria:plist-hash-table keys-and-values :test 'equal))

(defun response-json (&rest keys-and-values)
  (setf (hunchentoot:content-type*) "application/json"
        (hunchentoot:return-code*) 200)
  (loop with ht = (make-hash-table :test 'equal)
        for (key value) on keys-and-values by #'cddr
        do (setf (gethash (etypecase key
                            (string key)
                            (keyword (string-downcase (symbol-name key))))
                          ht)
                 value)
        finally (return (with-output-to-string (json)
                          (yason:encode ht json)))))
