(defpackage :clickhouse.http
  (:use :cl :dexador)
  (:shadow #:get
           #:post
           #:response-headers
           #:delete
           #:write-multipart-content)
  (:export :http-get
           :http-post))
  
(in-package :clickhouse.http)

(defun http-get (host-slot port-slot ssl-slot uri)
  (multiple-value-bind (body status response-headers uri stream)
      (dexador:get (clickhouse.utils:format-url host-slot port-slot ssl-slot uri)
		   :force-string t)
    (values body)))

(defun http-post (host-slot port-slot ssl-slot content timeout)
  (multiple-value-bind (body status response-header uri stream)
      (dexador:post (clickhouse.utils:format-url host-slot port-slot ssl-slot "")
		    :content content
		    :force-string t
		    :read-timeout (if timeout timeout 60))
    (values body)))
