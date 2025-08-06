;;;; ch-tests.lisp - Test Suite for ClickHouse Common Lisp Client
;;;; 
;;;; Usage: 
;;;;   (load "ch.lisp")
;;;;   (load "ch-test.lisp")
;;;;   (ch-tests:run-all-tests)
;;;;
;;;; Author: julio@clickhouse.com
;;;; License: Apache-2.0

(defpackage :ch-tests
  (:use :cl)
  (:export #:run-all-tests
           #:run-unit-tests
           #:run-integration-tests
           #:*test-host*
           #:*test-port*
           #:*test-username*
           #:*test-password*))

(in-package :ch-tests)

