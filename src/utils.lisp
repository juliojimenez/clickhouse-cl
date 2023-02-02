(defpackage :clickhouse.utils
  (:use :cl)
  (:export :format-url
           :prettify
           :ver))

(in-package :clickhouse.utils)

(defun format-url (host-slot port-slot ssl-slot uri)
  "Formats a URL"
  (cond (ssl-slot (format nil "https://~a:~a~a" host-slot port-slot uri))
	((not ssl-slot) (format nil "http://~a:~a~a" host-slot port-slot uri))
	(t (format nil "https://~a:~a~a" host-slot port-slot uri))))

(defun prettify (body &key console formatting)
  "Output conditioner and formatter resolver."
  (let ((b (string-trim '(#\Newline) body)))
    (cond (console (format t "~d" b))
	  ((ver formatting) (cond ((equalp formatting clickhouse.ch-sql-parser::'json) (clickhouse.ch-sql-parser:json-formats b))))
	  (t (values b)))))

(defun ver (val)
  "Boolean coercion helper."
  (not (not val)))
