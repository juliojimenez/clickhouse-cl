(defpackage :clickhouse.http
  (:use :cl :dexador)
  (:export :http-get
	   :http-post)
  (:shadowing-import-from :dexador "GET")
  (:shadowing-import-from :dexador "DELETE"))
  
(in-package :clickhouse.http)

(defun format-url (host-slot port-slot ssl-slot uri)
  (cond (ssl-slot (format nil "https://~a:~a~a" host-slot port-slot uri))
	((not ssl-slot) (format nil "http://~a:~a~a" host-slot port-slot uri))
	(t (format nil "https://~a:~a~a" host-slot port-slot uri))))

(defun http-get (host-slot port-slot ssl-slot uri)
  (dexador:get (format-url host-slot port-slot ssl-slot uri)))

(defun http-post (host-slot port-slot ssl-slot uri content)
  (dexador:post (format-url host-slot port-slot ssl-slot uri)
		:content content))
