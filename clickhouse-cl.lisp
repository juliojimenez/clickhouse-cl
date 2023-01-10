(defpackage :clickhouse
  (:nicknames :ch)
  (:use :cl :sxql :dexador)
  (:shadowing-import-from :dexador "GET")
  (:shadowing-import-from :dexador "DELETE")
  (:export :connect
	   :database))

(in-package :clickhouse)

(defun connect ()
  "Connect to ClickHouse Instance"
  (print "Connected! (not really)"))

(defclass database ()
  ((host
    :initarg :host)
   (port
    :initarg :port
    :initform 8443)
   (username
    :initarg :username
    :initform "default")
   (password
    :initarg :password)))

