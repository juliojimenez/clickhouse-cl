(defpackage :clickhouse.http
  (:use :cl)
  (:shadowing-import-from :dexador "GET" "POST")
  (:shadowing-import-from :dexador.body "WRITE-MULTIPART-CONTENT")
  (:import-from :clickhouse.utils
		            :format-url)
  (:export :http-get
           :http-post))
  
(in-package :clickhouse.http)

(defun http-get (host-slot port-slot ssl-slot uri)
  (multiple-value-bind (body status response-headers uri stream)
      (dexador:get (format-url host-slot port-slot ssl-slot uri)
		   :force-string t)
    (values body)))

(defun http-post (host-slot port-slot ssl-slot content timeout)
  (multiple-value-bind (body status response-header uri stream)
      (dexador:post (format-url host-slot port-slot ssl-slot "")
		    :content content
		    :force-string t
		    :read-timeout (if timeout timeout 60))
    (values body)))
