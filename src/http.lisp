(defpackage :clickhouse.http
  (:use :cl)
  (:shadowing-import-from "DEXADOR" "GET")
  (:shadowing-import-from "DEXADOR" "POST")
  (:shadowing-import-from "DEXADOR" "DELETE")
  (:export :http-get
           :http-post))
  
(in-package :clickhouse.http)

(defun http-get (host-slot port-slot ssl-slot uri)
  "HTTP handler for GET endpoints."
  (multiple-value-bind (body status response-headers uri stream)
      (get (clickhouse.utils:format-url host-slot port-slot ssl-slot uri)
		   :force-string t)
    (values body)))

(defun http-post (host-slot port-slot ssl-slot user-slot pass-slot content timeout)
  "HTTP handler for POST endpoints."
  (multiple-value-bind (body status response-header uri stream)
      (post (clickhouse.utils:format-url host-slot port-slot ssl-slot "")
        :basic-auth (user-pass user-slot pass-slot)
		    :content content
		    :force-string t
		    :read-timeout (if timeout timeout 60))
    (values body)))

(defun user-pass (user pass)
  (if (and user pass)
      (cons user pass)
      nil))
