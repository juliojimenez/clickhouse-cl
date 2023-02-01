(defpackage :clickhouse-test
  (:use :cl :fiveam)
  (:import-from :clickhouse.utils
                :format-url
                :ver))

(in-package :clickhouse-test)

(def-suite all-tests :description "all tests suite.")

(in-suite all-tests)

; clickhouse.utils

(test format-url-non-ssl
  (is (string= "http://localhost:8123" (format-url "localhost" 8123 nil ""))))

(test format-url-ssl
  (is (string= "https://localhost:8123" (format-url "localhost" 8123 t ""))))

(test format-url-ssl-uri
  (is (string= "https://localhost:8443/ping" (format-url "localhost" 8443 t "/ping"))))

(test ver-t
  (is (equalp t (ver "okie dokie"))))

(test ver-nil
  (is (equalp nil (ver nil))))

(test prettify-nil
  (is (string= "1." (clickhouse.utils:prettify "1." :console nil))))

(test prettify-console
  (is (equalp nil (clickhouse.utils:prettify "1." :console t))))

; clickhouse.ch-sql-parser

(test make-query
  (is (string= "SELECT 1" (clickhouse.ch-sql-parser:make-query "SELECT 1"))))

; database class and methods

(test make-database
  (finishes (defparameter *db-test* (make-instance 'clickhouse:database))))

(test ping-implicit
  (is (string= "Ok." (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test*)))))

(test ping-explicit
  (is (string= "Ok." (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test* :ping t)))))

(test ping-implicit-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test* :console t)))))

(test ping-explicit-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test* :ping t :console t)))))

(test replicas-status
  (is (string= "Ok." (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:replicas-status *db-test*)))))

(test replica-status-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			 (clickhouse:replicas-status *db-test* :console t)))))

(test easy-query
  (is (string= "1" (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			  (clickhouse:query *db-test* "SELECT 1")))))

(test easy-query-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			 (clickhouse:query *db-test* "SELECT 1" :console t)))))
