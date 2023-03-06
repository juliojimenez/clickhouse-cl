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
  (:import-from :cl-ppcre
                :regex-replace)
  (:export :database
           :input-parameters
           :jget
           :ping
           :query
           :replicas-status))

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
  (:documentation "Pings the database server."))

(defmethod ping ((obj database) &key ping console)
  "Pings the database server."
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
      (if (ver ping)
	 (http-get h p s "/ping")
	 (http-get h p s "/"))
     :console console)))

(defgeneric replicas-status (obj &key)
  (:documentation "Get replicas status."))

(defmethod replicas-status ((obj database) &key console verbose)
  "Get replicas status."
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
      (cond (verbose (http-get h p s "/replicas_status?verbose=1"))
            (t       (http-get h p s "/replicas_status")))
      :console console)))

(defgeneric query (obj query &key)
  (:documentation "Execute a query."))

(defmethod query ((obj database) query &key console no-format timeout)
  "Execute a query."
  (with-slots ((h host) (p port) (s ssl)) obj
    (prettify
     (http-post h p s (make-query query) timeout)
     :console console :formatting (if no-format nil clickhouse.ch-sql-parser:*format*))))

(defmacro jget (obj key)
  "Get JSON value."
  `(boost-json:json-getf ,obj ,key))

(defun input-parameters (query &rest input)
  (let ((new-query query))
    (dolist (in input)
      (setf new-query (regex-replace "\\$i" new-query in)))
    (values new-query)))
