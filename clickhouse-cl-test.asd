(asdf:defsystem #:clickhouse-cl-test
  :description "clickhouse-cl Tests"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.0.4"
  :depends-on (#:clickhouse-cl
	       #:fiveam)
  :components ((:module "t"
			:components
			((:file "clickhouse-cl"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :clickhouse-cl)))
