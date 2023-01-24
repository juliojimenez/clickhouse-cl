(in-package :cl-user)

(defpackage ch-asd
  (:use :cl :asdf))

(in-package :ch-asd)

(asdf:defsystem #:clickhouse-cl
  :description "Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.0.5"
  :serial t
  :depends-on (#:dexador)
  :components ((:file "clickhouse-cl")
	       (:file "ch-sql-parser")
	       (:file "http")))
