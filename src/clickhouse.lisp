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
                :string-to-list
                :ver
                :client-id)
  (:import-from :cl-ppcre
                :regex-replace)
  (:import-from :usocket
                :socket-connect
                :socket-stream
                :wait-for-input
                :listen)
  (:import-from :uuid
                :make-v4-uuid)
  (:export :database
           :input-parameters
           :jget
           :connect
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
    :documentation "ClickHouse database port, i.e. 8443 or 8123 for HTTP, 9000 or 9440 for TCP (native).")
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
    :initform ""
    :accessor password
    :documentation "Clickhouse database password.")))

(defparameter *socket* nil)
(defparameter *stream* nil)
(defparameter *client* "clickhouse-cl")
(defparameter *client-id* nil)
(defparameter *major-version* 0)
(defparameter *minor-version* 44)
(defparameter *revision* 54465)

(defgeneric connect (obj)
  (:documentation "Connect to ClickHouse Binary Protocol"))

(defmethod connect ((obj database))
  "Connect to binary protocol"
  (let ((raw-response nil)
        (string-response (make-array 0
                            :element-type 'character
                            :fill-pointer 0
                            :adjustable t)))
    (with-slots ((h host) (p port) (s ssl) (u username) (w password)) obj
      (setq *socket* (usocket:socket-connect h p :element-type '(unsigned-byte 8)))
      (setq *stream* (usocket:socket-stream *socket*))
      (hello u w)
      (setq raw-response (loop for c = (read-byte *stream* nil nil)
                               while (listen *stream*)
                               collect c))
      (dolist (character raw-response)
        (if (and (> character 31) (< character 128))
            (vector-push-extend (code-char character) string-response))))
      (setq *client-id* (client-id string-response))))

(defgeneric ping (obj &key)
  (:documentation "Pings the database server."))

(defmethod ping ((obj database) &key ping console)
  "Pings the database server."
  (with-slots ((h host) (p port) (s ssl)) obj
    (if (< p 9000)
      (prettify
        (if (ver ping)
          (http-get h p s "/ping")
          (http-get h p s "/"))
        :console console)
      (print "Not implemented yet for Binary Protocol"))))

(defgeneric replicas-status (obj &key)
  (:documentation "Get replicas status."))

(defmethod replicas-status ((obj database) &key console verbose)
  "Get replicas status."
  (with-slots ((h host) (p port) (s ssl)) obj
    (if (< p 9000)
      (prettify
        (cond (verbose (http-get h p s "/replicas_status?verbose=1"))
              (t       (http-get h p s "/replicas_status")))
        :console console)
      (print "Not implemented yet for Binary Protocol"))))

(defgeneric query (obj query &key)
  (:documentation "Execute a query."))

(defmethod query ((obj database) query &key console no-format timeout)
  "Execute a query."
  (with-slots ((h host) (p port) (s ssl) (u username) (w password)) obj
    (if (< p 9000)
      (prettify
        (http-post h p s u w (make-query query) timeout)
        :console console :formatting (if no-format nil clickhouse.ch-sql-parser:*format*))
      (progn
        (let ((raw-response nil)
              (string-response (make-array 0
                                  :element-type 'character
                                  :fill-pointer 0
                                  :adjustable t)))
          (prepare-query query)
          (setq raw-response (loop for c = (read-byte *stream* nil nil)
                                   while (listen *stream*)
                                   collect c))
          (print "raw")
          (print raw-response)
          (dolist (character raw-response)
            (if (and (> character 31) (< character 128))
                (vector-push-extend (code-char character) string-response)))
          (print string-response)
          (setq *client-id* string-response))))))

(defmacro jget (obj key)
  "Get JSON value."
  `(boost-json:json-getf ,obj ,key))

(defun input-parameters (query &rest input)
  (let ((new-query query))
    (dolist (in input)
      (setf new-query (regex-replace "\\$i" new-query in)))
    (values new-query)))

(defun hello (u w)
  (let* ((client-char-list (string-to-list *client*))
         (db-char-list (string-to-list ""))
         (user-char-list (string-to-list u))
         (pass-char-list (string-to-list w)))
    ;; Hello - Client handshake start
    (write-byte 0 *stream*)
    ;; Client Name - i.e. clickhouse-cl
    (write-byte (length client-char-list) *stream*)
    (dolist (character client-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    ;; Major Version
    (write-byte *major-version* *stream*)
    ;; Minor Version
    (write-byte *minor-version* *stream*)
    ;; Revision
    (write-byte (ldb (byte 8 8) *revision*) *stream*)
    (write-byte (ldb (byte 8 0) *revision*) *stream*)
    ;; What does this 3 mean? Idk.
    (write-byte 3 *stream*)
    ;; Database Name
    (if (> (length db-char-list) 0)
        (dolist (character db-char-list)
          (let ((charcode (char-code character)))
            (write-byte charcode *stream*)))
        (write-byte 0 *stream*))
    ;; User
    (write-byte (length u) *stream*)
    (dolist (character user-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    ;; Password
    (write-byte (length w) *stream*)
    (dolist (character pass-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    (force-output *stream*)))

(defun prepare-query (query)
  (let* ((query-list (string-to-list query))
         (query-id (format nil "~(~a~)" (make-v4-uuid)))
         (query-id-char-list (string-to-list query-id))
         (host-ip-char-list (string-to-list "0.0.0.0:0"))
         (client-id-char-list (string-to-list *client-id*))
         (client-char-list (string-to-list *client*)))
    ;; Query
    (write-byte 1 *stream*)
    ;; Query ID (UUID)
    (write-byte (length query-id-char-list) *stream*)
    (dolist (character query-id-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    ;; Query Kind (None=0, Initial=1, Secondary=2) Should this be 0?
    (write-byte 1 *stream*)
    ;; Initial User
    (write-byte 0 *stream*)
    ;; Query ID
    (write-byte (length query-id-char-list) *stream*)
    (dolist (character query-id-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    ;; Initial Address - 0.0.0.0:0
    (write-byte (length host-ip-char-list) *stream*)
    (dolist (character host-ip-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    ;; Initial time - 0 
    (dotimes (n 8)
      (write-byte 0 *stream*))
    ;; TCP Protocol - 1
    (write-byte 1 *stream*)
    ;; Client Host Name - 0 (as in nada)
    (write-byte 0 *stream*)
    (write-byte (length client-id-char-list) *stream*)
    (dolist (character client-id-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    (write-byte (length client-char-list) *stream*)
    (dolist (character client-char-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    (write-byte 0 *stream*)
    (write-byte 44 *stream*)
    (write-byte (ldb (byte 8 8) 54465) *stream*)
    (write-byte (ldb (byte 8 0) 54465) *stream*)
    (write-byte 3 *stream*)
    (dotimes (n 2)
      (write-byte 0 *stream*))
    (write-byte 4 *stream*)
    (dotimes (n 6)
      (write-byte 0 *stream*))
    (write-byte 2 *stream*)
    (write-byte 0 *stream*)
    (write-byte (length query-list) *stream*)
    (dolist (character query-list)
      (let ((charcode (char-code character)))
        (write-byte charcode *stream*)))
    (write-byte 0 *stream*)
    (write-byte 2 *stream*)
    (write-byte 0 *stream*)
    (write-byte 1 *stream*)
    (write-byte 0 *stream*)
    (write-byte 2 *stream*)
    (dotimes (n 4)
      (write-byte 255 *stream*))
    (dotimes (n 3)
      (write-byte 0 *stream*))
    (force-output *stream*)))
