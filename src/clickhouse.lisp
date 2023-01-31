(defpackage :clickhouse
  (:nicknames :ch)
  (:use :cl)
  (:import-from :clickhouse.ch-sql-parser
                :make-query)
  (:import-from :clickhouse.http
                :http-get
                :http-post)
  (:import-from :clickhouse.utils
                :prettify
                :ver)
  (:export :database
           :ping
           :replicas-status
           :query
           :jget))

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

(defmethod ping ((obj database) &key ping console)
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
     (if (ver ping)
	 (http-get h p s "/ping")
	 (http-get h p s "/"))
     :console console)))

(defgeneric replicas-status (obj &key)
  (:documentation "Get replicas status."))

(defmethod replicas-status ((obj database) &key console)
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
     (http-get h p s "/replicas_status")
     :console console)))

(defgeneric query (obj query &key)
  (:documentation "Execute a query"))

(defmethod query ((obj database) query &key console formatting)
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
     (http-post h p s (make-query query))
     :console console :formatting clickhouse.ch-sql-parser:*format*)))

(defmacro jget (obj key)
  `(boost-json:json-getf ,obj ,key))

