(defpackage :clickhouse.http
  (:use :cl)
  (:shadowing-import-from "DEXADOR" "GET")
  (:shadowing-import-from "DEXADOR" "POST")
  (:shadowing-import-from "DEXADOR" "DELETE")
  (:shadowing-import-from "DEXADOR.BACKEND.USOCKET" "WRITE-MULTIPART-CONTENT")
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
