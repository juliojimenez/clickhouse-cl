(defpackage :clickhouse.tcp
  (:use :cl)
  (:import-from :usocket
                :socket-connect)
  (:export :native))
  
(in-package :clickhouse.tcp)

(defparameter *socket* nil)

(defclass native (database)
  ())

(defgeneric connect (obj)
  (:documentation "Connect to ClickHouse. Client Hello."))

(defmethod connect ((obj database))
  "Connect to ClickHouse. Client Hello."
  (with-slots ((h host) (p port) (s ssl) (u username) (w password)) obj
    (setq *socket* (usocket:socket-connect h p))))

(defun write-and-flush (string)
  (write-string string str)
  (force-output str))