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

(defun prettify (body &key console)
  (cond (console (format t "~d" (string-trim '(#\Newline) body)))
	(t (string-trim '(#\Newline) body))))

(defmacro ver (val)
  `(not (not ,val)))

;(defmacro coerce-by-length (input)
;  (if (= 1 (length input))
;      (coerce input 'character)
;      (coerce input 'string)))
