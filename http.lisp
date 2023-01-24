(in-package :cl-user)
(defpackage :http
  (:use :cl)
  (:export :http-get
	   :http-post))

(in-package :http)

;(defparameter *query-string* nil)

;(defun make-query (query)
;  (setf *query-string* query))

