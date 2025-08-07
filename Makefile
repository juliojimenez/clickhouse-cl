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

ql-install-ci:
	curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(SBCL) --non-interactive --no-sysinit --no-userinit \
		--load /tmp/ql.lisp \
		--eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
		--eval '(with-open-file (s "~/.sbclrc" :direction :output :if-exists :append :if-does-not-exist :create) (format s "(load \"~~/.quicklisp/setup.lisp\")~%"))' \
		--quit

.PHONY: build clean ql-check ql-check-ci ql-install ql-install-ci
