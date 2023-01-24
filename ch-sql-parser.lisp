(in-package :cl-user)
(defpackage :ch-sql-parser
  (:nicknames :ch-sql)
  (:use :cl)
  (:export :make-query))

(in-package :ch-sql-parser)

(defparameter *query-string* nil)

(defun make-query (query)
  (setf *query-string* query))
