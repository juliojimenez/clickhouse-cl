(defpackage :clickhouse.utils
  (:use :cl)
  (:export :format-url
           :prettify
           :ver))

(in-package :clickhouse.utils)

(defun format-url (host-slot port-slot ssl-slot uri)
  (cond (ssl-slot (format nil "https://~a:~a~a" host-slot port-slot uri))
	((not ssl-slot) (format nil "http://~a:~a~a" host-slot port-slot uri))
	(t (format nil "https://~a:~a~a" host-slot port-slot uri))))

(defun prettify (body &key nice nicer)
  (cond (nice (format nil "~d" (string-trim '(#\Newline #\") body)))
	(nicer (format t "~d" (remove #\\ (remove #\' (string-trim '(#\Newline #\") body)))))
	(t (format nil "~s" (string-trim '(#\Newline #\") body)))))

(defmacro ver (val)
  `(not (not ,val)))
