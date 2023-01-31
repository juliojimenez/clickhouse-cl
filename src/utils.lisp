(defpackage :clickhouse.utils
  (:use :cl)
  (:export :format-url
           :prettify
           :ver
	   :coerce-by-length))

(in-package :clickhouse.utils)

(defun format-url (host-slot port-slot ssl-slot uri)
  (cond (ssl-slot (format nil "https://~a:~a~a" host-slot port-slot uri))
	((not ssl-slot) (format nil "http://~a:~a~a" host-slot port-slot uri))
	(t (format nil "https://~a:~a~a" host-slot port-slot uri))))

(defun prettify (body &key console formatting)
  (let ((b (string-trim '(#\Newline) body)))
    (cond (console (format t "~d" b))
	  ((ver formatting) (cond ((equalp formatting clickhouse.ch-sql-parser::'jsoneachrow) (clickhouse.ch-sql-parser:json-each-row b))))
	  (t (values b)))))

(defmacro ver (val)
  `(not (not ,val)))
