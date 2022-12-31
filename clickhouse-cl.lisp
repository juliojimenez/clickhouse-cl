(defpackage :clickhouse
  (:nicknames :ch)
  (:use :cl :sxql :dexador)
  (:shadowing-import-from :dexador "GET")
  (:shadowing-import-from :dexador "DELETE")
  (:export :connect))

(in-package :clickhouse)

(defun connect ()
  "Connect to ClickHouse Instance"
  (print "Connected! (not really)"))
