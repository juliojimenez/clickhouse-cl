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
  (terpri)
  (let ((b (string-trim '(#\Newline) body)))
    (cond (console (format t "~d" b))
	  ((ver formatting) (cond ((equalp formatting clickhouse.ch-sql-parser::'json)
				   (json-formats b))
				  ((equalp formatting clickhouse.ch-sql-parser::'tabseparated)
				   (tab-separated-formatter b))))
	  (t (values b)))))

(defun ver (val)
  "Boolean coercion helper."
  (not (not val)))

(defun json-formats (input)
  "Decodes input into a BOOST-JSON:JSON-OBJECT."
  (boost-json:json-decode input))

(defun tab-separated-formatter (input)
  (setq tab-separated nil)
  (dolist (x (uiop:split-string input :separator '(#\Newline)))
    (push (uiop:split-string x :separator '(#\Tab)) tab-separated))
  (values tab-separated))
