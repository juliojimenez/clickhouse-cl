(asdf:defsystem #:clickhouse-cl
  :description "Common Lisp ClickHouse Client Library"

  :author "julio@clickhouse.com"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:sxql #:dexador)
  :components ((:file "clickhouse-cl")))
