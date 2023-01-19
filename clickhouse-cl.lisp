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
    :initform "localhost"
    :accessor host
    :documentation "ClickHouse database hostname.")
   (port
    :initarg :port
    :initform 8123
    :accessor port
    :documentation "ClickHouse database port, i.e. 8443 or 8123.")
   (ssl
    :initarg :ssl
    :initform nil
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

(defgeneric ping (obj &key)
  (:documentation "Pings the database server"))

(defmethod ping ((obj database) &key ping)
  (with-slots ((h host) (p port) (s ssl)) obj
    (if (not (not ping))
	(http-get h p s "/ping")
	(http-get h p s "/"))))

(defgeneric replicas-status (obj)
  (:documentation "Get replicas status."))

(defmethod replicas-status ((obj database))
  (with-slots ((h host) (p port) (s ssl)) obj
    (http-get h p s "/replicas_status")))

(defgeneric query (obj &key sxql raw)
  (:documentation "Execute a query"))

(defmethod query ((obj database) &key sxql raw)
  (with-slots ((h host) (p port) (s ssl)) obj
    (http-get h p s query)))

(defun format-url (host-slot port-slot ssl-slot uri)
  (cond (ssl-slot (format nil "https://~a:~a~a" host-slot port-slot uri))
	((not ssl-slot) (format nil "http://~a:~a~a" host-slot port-slot uri))
	(t (format nil "https://~a:~a~a" host-slot port-slot uri))))

(defun http-get (host-slot port-slot ssl-slot uri)
  (dexador:get (format-url host-slot port-slot ssl-slot uri)))

(defmacro sql-parser (verb field @body body clauses)
  `(,verb field ,@body clauses))
