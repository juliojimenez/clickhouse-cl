(asdf:defsystem #:clickhouse-test
  :description "clickhouse Tests"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.0.8"
  :depends-on (#:clickhouse
	       #:fiveam)
  :components ((:module "t"
		:components
		((:file "clickhouse"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! 'clickhouse-test::all-tests)))
