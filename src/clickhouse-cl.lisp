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
                :ver
		:array-vector-string)
  (:export :database
           :ping
           :replicas-status
	   :query))

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

(defmethod ping ((obj database) &key ping nice nicer)
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
     (if (ver ping)
	 (http-get h p s "/ping")
	 (http-get h p s "/"))
     :nice nice :nicer nicer)))

(defgeneric replicas-status (obj &key)
  (:documentation "Get replicas status."))

(defmethod replicas-status ((obj database) &key nice nicer )
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
     (array-vector-string (http-get h p s "/replicas_status"))
     :nice nice :nicer nicer)))

(defgeneric query (obj query &key)
  (:documentation "Execute a query"))

(defmethod query ((obj database) query &key nice nicer)
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
     (http-post h p s (make-query query))
     :nice nice :nicer nicer)))

