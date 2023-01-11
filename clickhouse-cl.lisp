(defpackage :clickhouse
  (:nicknames :ch)
  (:use :cl :sxql :dexador)
  (:shadowing-import-from :dexador "GET")
  (:shadowing-import-from :dexador "DELETE")
  (:export :database))

(in-package :clickhouse)

(defun connect ()
  "Connect to ClickHouse Instance"
  (print "Connected! (not really)"))

(defclass database ()
  ((host
    :initarg :host
    :initform (error "Must provide a hostname.")
    :accessor host
    :documentation "ClickHouse database hostname.")
   (port
    :initarg :port
    :initform 8443
    :accessor port
    :documentation "ClickHouse database port, i.e. 8443 or 8123.")
   (username
    :initarg :username
    :initform "default"
    :accessor username
    :documentation "ClickHouse database username, default username is default.")
   (password
    :initarg :password
    :accessor password
    :documentation "Clickhouse database password.")))

