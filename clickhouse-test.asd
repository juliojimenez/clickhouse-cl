(asdf:defsystem #:clickhouse-test
  :description "clickhouse Tests"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.0.16"
  :depends-on (#:clickhouse
	       #:fiveam)
  :components ((:module "t"
		:components
		((:file "clickhouse"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! (find-symbol "ALL-TESTS" 'clickhouse-test))))
