SBCL=sbcl

load:
	$(SBCL) --load ch.lisp

unit-tests:
	$(SBCL) --load ch.lisp --load ch-test.lisp --eval '(ch-tests:run-unit-tests)' --eval '(ch-tests:print-test-summary)' --quit

integration-tests:
	$(SBCL) --load ch.lisp --load ch-test.lisp --eval '(ch-tests:run-integration-tests)' --eval '(ch-tests:print-test-summary)' --quit

performance-tests: 
	$(SBCL) --load ch.lisp --load ch-test.lisp --eval '(ch-tests:run-performance-tests)' --eval '(ch-tests:print-test-summary)' --quit

all-tests:
	$(SBCL) --load ch.lisp --load ch-test.lisp --eval '(ch-tests:run-all-tests)' --quit

basic-connection:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/basic-connection.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit

data-insertion:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/data-insertion.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit

formats:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/formats.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit

error-handling:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/error-handling.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit

clickhouse-cloud:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/clickhouse-cloud.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit

clickhouse-cloud-real:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/clickhouse-cloud-real.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit

performance:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/performance.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit
