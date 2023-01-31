(asdf:defsystem #:clickhouse
  :description "Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.1.5"
  :depends-on (#:dexador
	       #:lexer
	       #:boost-json)
  :components ((:module "src"
		:components
		((:file "utils" :depends-on ("ch-sql-parser"))
		 (:file "ch-sql-parser")
		 (:file "http")
		 (:file "clickhouse" :depends-on ("http" "ch-sql-parser")))))
  :in-order-to ((test-op (test-op "clickhouse-cl-test"))))


(asdf:defsystem #:clickhouse/ci
  :description "CI for Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :depends-on ("40ants-ci"
	       "40ants-doc"
	       "40ants-doc/changelog")
  :components ((:module "src"
		:components
		((:file "ci")))))
