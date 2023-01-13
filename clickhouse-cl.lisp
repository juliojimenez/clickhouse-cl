(defpackage :clickhouse
  (:nicknames :ch)
  (:use :cl :sxql :dexador)
  (:shadowing-import-from :dexador "GET")
  (:shadowing-import-from :dexador "DELETE")
  (:export :database))

(in-package :clickhouse)

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
   (ssl
    :initarg :ssl
    :initform t
    :accessor ssl
    :documentation "SSL option, t or nil.")
   (username
    :initarg :username
    :initform "default"
    :accessor username
    :documentation "ClickHouse database username, default username is default.")
   (password
    :initarg :password
    :accessor password
    :documentation "Clickhouse database password.")))

(defmethod ping ((obj database))
  (with-slots ((ssl ssl) (host host) (port port)) obj
    (dexador:get (format-url ssl host port))))

(defun format-url (ssl host port)
  (cond ((ssl) (format nil "https://~a:~a" host port))
	((not ssl) (format nil "http://~a:~a" host port))
	(t (format nil "https://~a:~a" host port))))
