(defpackage :clickhouse.ch-sql-parser
  (:use cl)
  (:export :make-query))

(in-package :clickhouse.ch-sql-parser)
  
(defparameter *query-string* nil)

(defun make-query (query)
  (setf *query-string* query))
