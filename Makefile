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

formats:
	$(SBCL) --load ch.lisp --eval '(handler-case (load "examples/formats.lisp") (error (e) (format t "ERROR: ~A~%" e) (sb-ext:exit :code 1)))' --quit

#TODO: Adjust the rest of the Makefile for clickhouse-cl
# I lifted this from another one of my projects, so it may not be fully applicable.
ql-check-ci:
	@$(SBCL) --non-interactive \
	         --eval '(handler-case (progn (load "~/.quicklisp/setup.lisp") (format t "Quicklisp is installed.~%")) (error () (format t "Quicklisp is missing.~%") (sb-ext:exit :code 1)))'

ql-install:
	curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(SBCL) --no-sysinit --no-userinit \
		--load /tmp/ql.lisp \
		--eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
		--eval '(ql:add-to-init-file)' \
		--quit
