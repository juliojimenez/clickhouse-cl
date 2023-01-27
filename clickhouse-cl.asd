(asdf:defsystem #:clickhouse-cl
  :description "Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.1.0"
  :depends-on (#:dexador)
  :components ((:module "src"
			:components
			((:file "utils")
			 (:file "ch-sql-parser")
			 (:file "http")
			 (:file "clickhouse-cl")))))
