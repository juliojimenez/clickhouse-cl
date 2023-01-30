(asdf:defsystem #:clickhouse-cl
  :description "Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.1.4"
  :depends-on (#:dexador)
  :components ((:module "src"
			:components
			((:file "utils")
			 (:file "ch-sql-parser" :depends-on ("lexer"))
			 (:file "http")
                         (:file "parse")
                         (:file "re" :depends-on ("parse"))
                         (:file "lexer" :depends-on ("re"))
			 (:file "clickhouse-cl"))))
  :in-order-to ((test-op (test-op "clickhouse-cl-test"))))


(asdf:defsystem #:clickhouse-cl/ci
  :description "CI for Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :depends-on (#:40ants-ci)
  :components ((:module "src"
		:components
		((:file "ci")))))
