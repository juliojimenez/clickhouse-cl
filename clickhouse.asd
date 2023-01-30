(asdf:defsystem #:clickhouse
  :description "Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.1.4"
  :depends-on (#:dexador
	       #:lexer
	       #:40ants-ci)
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
