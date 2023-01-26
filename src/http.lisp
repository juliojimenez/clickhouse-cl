(defpackage :clickhouse.http
  (:use :cl :dexador)
  (:import-from :clickhouse.utils
		:format-url)
  (:export :http-get
	   :http-post)
  (:shadowing-import-from :dexador "GET")
  (:shadowing-import-from :dexador "DELETE"))
  
(in-package :clickhouse.http)

(defun http-get (host-slot port-slot ssl-slot uri)
  (multiple-value-bind (body status response-headers uri stream)
      (dexador:get (format-url host-slot port-slot ssl-slot uri))
    (values body)))

(defun http-post (host-slot port-slot ssl-slot content)
  (multiple-value-bind (body status response-header uri stream)
      (dexador:post (format-url host-slot port-slot ssl-slot "")
		    :content content)
    (values body)))
