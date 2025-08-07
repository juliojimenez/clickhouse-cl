;;;; ch-tests.lisp - Test Suite for ClickHouse Common Lisp Client
;;;; 
;;;; Usage: 
;;;;   (load "ch.lisp")
;;;;   (load "ch-tests.lisp")
;;;;   (ch-tests:run-all-tests)
;;;;
;;;; Author: julio@clickhouse.com
;;;; License: Apache-2.0

(defpackage :ch-tests
  (:use :cl)
  (:export #:run-all-tests
           #:run-unit-tests
           #:run-integration-tests
           #:example-usage
           #:*test-host*
           #:*test-port*
           #:*test-username*
           #:*test-password*))

(in-package :ch-tests)

;;;; ============================================================================
;;;; TEST CONFIGURATION
;;;; ============================================================================

(defparameter *test-host* "localhost"
              "ClickHouse test server hostname")

(defparameter *test-port* 8123
              "ClickHouse test server port")

(defparameter *test-username* "default"
              "ClickHouse test username")

(defparameter *test-password* nil
              "ClickHouse test password")

(defparameter *test-results* '()
              "List to store test results")

;;;; ============================================================================
;;;; SIMPLE TEST FRAMEWORK
;;;; ============================================================================

(defmacro deftest (name description &body body)
  "Define a test case."
  `(defun ,name ()
     (let ((test-name ,(string name))
           (test-description ,description)
           (start-time (get-internal-real-time)))
       (format t "~&Testing ~A: ~A... " test-name test-description)
       (handler-case
           (progn
            ,@body
            (let ((duration (/ (- (get-internal-real-time) start-time)
                               internal-time-units-per-second)))
              (format t "PASS (~,3Fs)~%" duration)
              (push (list test-name :pass test-description duration) *test-results*)
              t))
         (error (e)
           (let ((duration (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))
             (format t "FAIL (~,3Fs)~%" duration)
             (format t "    Error: ~A~%" e)
             (push (list test-name :fail test-description duration e) *test-results*)
             nil))))))

(defmacro assert-equal (expected actual &optional description)
  "Assert that two values are equal."
  `(unless (equal ,expected ,actual)
     (error "Assertion failed~@[ (~A)~]: expected ~S, got ~S"
       ,description ,expected ,actual)))

(defmacro assert-true (condition &optional description)
  "Assert that condition is true."
  `(unless ,condition
     (error "Assertion failed~@[ (~A)~]: expected true, got ~S"
       ,description ,condition)))

(defmacro assert-error (condition &optional description)
  "Assert that an error is signaled."
  `(let ((error-signaled nil))
     (handler-case
         ,condition
       (error (e)
         (declare (ignore e))
         (setf error-signaled t)))
     (unless error-signaled
       (error "Assertion failed~@[ (~A)~]: expected error to be signaled"
         ,description))))

;;;; ============================================================================
;;;; UTILITY FUNCTIONS FOR TESTS
;;;; ============================================================================

(defun make-test-database (&key (host *test-host*) (port *test-port*)
                                (username *test-username*) (password *test-password*))
  "Create a test database connection."
  (ch:make-database :host host :port port
                    :username username :password password))

(defun clickhouse-available-p ()
  "Check if ClickHouse server is available for testing."
  (handler-case
      (let ((db (make-test-database)))
        (ch:ping db)
        t)
    (error () nil)))

;;;; ============================================================================
;;;; UNIT TESTS - NO CLICKHOUSE SERVER REQUIRED
;;;; ============================================================================

(deftest test-database-creation
    "Database object creation"
  (let ((db (ch:make-database :host "test-host" :port 9999)))
    (assert-equal "test-host" (ch::database-host db))
    (assert-equal 9999 (ch::database-port db))
    (assert-equal "default" (ch::database-username db))))

(deftest test-json-parsing-simple
    "Simple JSON parsing"
  (let ((json-string "{\"key\": \"value\", \"number\": 42}"))
    (let ((parsed (ch::json-parse json-string)))
      (assert-equal "value" (ch:jget parsed "key"))
      (assert-equal 42 (ch:jget parsed "number")))))

(deftest test-json-parsing-array
    "JSON array parsing"
  (let ((json-string "[1, 2, 3, \"hello\"]"))
    (let ((parsed (ch::json-parse json-string)))
      (assert-equal 4 (length parsed))
      (assert-equal 1 (first parsed))
      (assert-equal "hello" (fourth parsed)))))

(deftest test-json-parsing-nested
    "Nested JSON parsing"
  (let ((json-string "{\"data\": {\"name\": \"test\", \"values\": [1, 2, 3]}}"))
    (let ((parsed (ch::json-parse json-string)))
      (let ((data (ch:jget parsed "data")))
        (assert-equal "test" (ch:jget data "name"))
        (assert-equal 3 (length (ch:jget data "values")))))))

(deftest test-split-string
    "String splitting utility"
  (assert-equal '("a" "b" "c") (ch::split-string "a,b,c" #\,))
  (assert-equal '("hello" "world") (ch::split-string "hello	world" #\Tab))
  (assert-equal '("hello" "world") (ch::split-string "hello world" #\Space))
  (assert-equal '("single") (ch::split-string "single" #\,))
  (assert-equal '("" "") (ch::split-string "," #\,)))

(deftest test-format-extraction
    "SQL FORMAT clause extraction"
  (assert-equal "JSON" (ch::extract-format-from-query "SELECT 1 FORMAT JSON"))
  (assert-equal "CSV" (ch::extract-format-from-query "select name from users format CSV"))
  (assert-equal "TabSeparated" (ch::extract-format-from-query "SHOW TABLES FORMAT TabSeparated"))
  (assert-equal nil (ch::extract-format-from-query "SELECT 1")))

(deftest test-csv-processing
    "CSV format processing"
  (let ((csv-response (format nil "name,age,city~%John,25,NYC~%Jane,30,LA")))
    (let ((processed (ch::process-csv-format csv-response)))
      (assert-equal 3 (length processed))
      (assert-equal '("name" "age" "city") (first processed))
      (assert-equal '("John" "25" "NYC") (second processed)))))

(deftest test-tab-separated-processing
    "TabSeparated format processing"
  (let* ((tab-char (string #\Tab))
         (newline-char (string #\Newline))
         (line1 (concatenate 'string "name" tab-char "age" tab-char "city"))
         (line2 (concatenate 'string "John" tab-char "25" tab-char "NYC"))
         (line3 (concatenate 'string "Jane" tab-char "30" tab-char "LA"))
         (tab-response (concatenate 'string line1 newline-char line2 newline-char line3)))
    (let ((processed (ch::process-tab-separated-format tab-response)))
      (assert-equal 3 (length processed))
      (assert-equal '("name" "age" "city") (first processed)))))

(deftest test-error-conditions
    "Error condition creation"
  (assert-error (error 'ch:clickhouse-error :message "test error"))
  (assert-error (error 'ch:connection-error :message "connection failed"))
  (assert-error (error 'ch:query-error :message "bad query" :query "SELECT invalid")))

;;;; ============================================================================
;;;; INTEGRATION TESTS - REQUIRE CLICKHOUSE SERVER
;;;; ============================================================================

(deftest test-ping-connection
    "Ping ClickHouse server"
  (let ((db (make-test-database)))
    (let ((result (ch:ping db)))
      (assert-true (stringp result))
      (assert-true (> (length result) 0)))))

(deftest test-simple-query
    "Execute simple SELECT query"
  (let ((db (make-test-database)))
    (let ((result (ch:query db "SELECT 1")))
      (let ((result-string (if (stringp result) result (format nil "~A" result))))
        (assert-true (search "1" result-string))))))

(deftest test-query-with-format
    "Execute query with explicit format"
  (let ((db (make-test-database)))
    (let ((result (ch:query db "SELECT 1 as num FORMAT JSON")))
      (assert-true (listp result))
      (let ((data-field (ch:jget result "data")))
        (assert-true data-field)
        (assert-true (listp data-field))
        (assert-equal 1 (length data-field))
        (let ((first-row (first data-field)))
          (assert-equal 1 (ch:jget first-row "num")))))))

(deftest test-version-query
    "Query ClickHouse version"
  (let ((db (make-test-database)))
    (let ((result (ch:query db "SELECT version()")))
      (assert-true (stringp result))
      (let ((trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) result)))
        (assert-true (> (length trimmed) 0))))))

(deftest test-show-databases
    "Show databases query"
  (let ((db (make-test-database)))
    (let ((result (ch:query db "SHOW DATABASES")))
      (assert-true (stringp result))
      (assert-true (search "system" result)))))

(deftest test-csv-format-query
    "Query with CSV format"
  (let ((db (make-test-database)))
    (let ((result (ch:query db "SELECT 'hello' as greeting, 42 as number FORMAT CSV")))
      (assert-true (listp result))
      (assert-true (>= (length result) 1))
      (let ((first-row (first result)))
        (assert-true (listp first-row))
        ;; CSV strings are quoted, so check for "hello" with quotes
        (assert-true (member "\"hello\"" first-row :test #'string=))
        (assert-true (member "42" first-row :test #'string=))))))

(deftest test-multiple-queries
    "Execute multiple queries in sequence"
  (let ((db (make-test-database)))
    (ch:query db "SELECT 1")
    (ch:query db "SELECT 2")
    (let ((result (ch:query db "SELECT 3")))
      (let ((result-string (if (stringp result) result (format nil "~A" result))))
        (assert-true (search "3" result-string))))))

(deftest test-query-with-numbers
    "Query with various number formats"
  (let ((db (make-test-database)))
    (let ((result (ch:query db "SELECT 1 as int, 3.14 as float, 'text' as str FORMAT JSON")))
      (let ((data (ch:jget result "data")))
        (assert-true (listp data))
        (assert-true (> (length data) 0))))))

(deftest test-raw-query
    "Raw query execution"
  (let ((db (make-test-database)))
    (let ((result (ch:query db "SELECT 1" :raw t)))
      (assert-true (stringp result))
      (assert-true (search "1" result)))))

;;;; ============================================================================
;;;; PERFORMANCE TESTS
;;;; ============================================================================

(deftest test-connection-performance
    "Test connection creation performance"
  (let ((start-time (get-internal-real-time)))
    (dotimes (i 10)
      (make-test-database))
    (let ((duration (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
      (assert-true (< duration 1.0) "Connection creation should be fast"))))

(deftest test-ping-performance
    "Test ping performance"
  (let ((db (make-test-database))
        (start-time (get-internal-real-time)))
    (dotimes (i 5)
      (ch:ping db))
    (let ((duration (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
      (assert-true (< duration 2.0) "Multiple pings should be reasonably fast"))))

;;;; ============================================================================
;;;; TEST RUNNERS
;;;; ============================================================================

(defparameter *unit-tests*
              '(test-database-creation
                test-json-parsing-simple
                test-json-parsing-array
                test-json-parsing-nested
                test-split-string
                test-format-extraction
                test-csv-processing
                test-tab-separated-processing
                test-error-conditions)
              "List of unit tests that don't require ClickHouse server")

(defparameter *integration-tests*
              '(test-ping-connection
                test-simple-query
                test-query-with-format
                test-version-query
                test-show-databases
                test-csv-format-query
                test-multiple-queries
                test-query-with-numbers
                test-raw-query)
              "List of integration tests that require ClickHouse server")

(defparameter *performance-tests*
              '(test-connection-performance
                test-ping-performance)
              "List of performance tests")

(defun run-test-suite (test-list suite-name)
  "Run a list of tests and return summary."
  (format t "~2%=== Running ~A ===~%" suite-name)
  (setf *test-results* '())
  (let ((passed 0)
        (failed 0))
    (dolist (test-name test-list)
      (if (funcall test-name)
          (incf passed)
          (incf failed)))
    (format t "~%~A Results: ~D passed, ~D failed~%"
      suite-name passed failed)
    (values passed failed)))

(defun run-unit-tests ()
  "Run unit tests only (no ClickHouse server required)."
  (run-test-suite *unit-tests* "Unit Tests"))

(defun run-integration-tests ()
  "Run integration tests (requires ClickHouse server)."
  (unless (clickhouse-available-p)
    (format t "~%WARNING: ClickHouse server not available at ~A:~A~%"
      *test-host* *test-port*)
    (format t "Skipping integration tests.~%")
    (return-from run-integration-tests (values 0 0)))
  (run-test-suite *integration-tests* "Integration Tests"))

(defun run-performance-tests ()
  "Run performance tests (requires ClickHouse server)."
  (unless (clickhouse-available-p)
    (format t "~%WARNING: ClickHouse server not available for performance tests~%")
    (return-from run-performance-tests (values 0 0)))
  (run-test-suite *performance-tests* "Performance Tests"))

(defun run-all-tests ()
  "Run all test suites."
  (format t "~%ClickHouse CL Test Suite~%")
  (format t "=============================~%")

  (multiple-value-bind (unit-passed unit-failed)
      (run-unit-tests)
    (multiple-value-bind (integration-passed integration-failed)
        (run-integration-tests)
      (multiple-value-bind (perf-passed perf-failed)
          (run-performance-tests)
        (let ((total-passed (+ unit-passed integration-passed perf-passed))
              (total-failed (+ unit-failed integration-failed perf-failed)))
          (format t "~2%=== FINAL RESULTS ===~%")
          (format t "Total: ~D passed, ~D failed~%" total-passed total-failed)
          (if (zerop total-failed)
              (format t "All tests passed!~%")
              (format t "~D test(s) failed~%" total-failed))
          (values total-passed total-failed))))))

(defun print-test-summary ()
  "Print detailed test results summary."
  (format t "~2%=== DETAILED TEST RESULTS ===~%")
  (dolist (result (reverse *test-results*))
    (destructuring-bind (name status description duration &optional error) result
      (format t "~A: ~A - ~A (~,3Fs)~%"
        name
        (if (eq status :pass) "PASS" "FAIL")
        description
        duration)
      (when error
            (format t "  Error: ~A~%" error)))))

;;;; ============================================================================
;;;; EXAMPLE USAGE AND DOCUMENTATION
;;;; ============================================================================

(defun example-usage ()
  "Show example usage of the test suite."
  (format t "~%ClickHouse CL Test Suite Usage:~%")
  (format t "================================~%")
  (format t "(load \"ch.lisp\")                    ; Load the main library~%")
  (format t "(load \"ch-tests.lisp\")              ; Load test suite~%")
  (format t "(ch-tests:run-all-tests)            ; Run all tests~%")
  (format t "(ch-tests:run-unit-tests)           ; Run unit tests only~%")
  (format t "(ch-tests:run-integration-tests)    ; Run integration tests~%")
  (format t "(ch-tests:print-test-summary)       ; Show detailed results~%")
  (format t "~%Test Configuration:~%")
  (format t "  *test-host*: ~S~%" *test-host*)
  (format t "  *test-port*: ~S~%" *test-port*)
  (format t "  *test-username*: ~S~%" *test-username*)
  (format t "~%To use different test server:~%")
  (format t "(setf ch-tests:*test-host* \"your-server\")~%")
  (format t "(setf ch-tests:*test-port* 8443)~%")
  (format t "(setf ch-tests:*test-username* \"testuser\")~%"))

;; Print usage information when loaded
(format t "~%ClickHouse CL Test Suite loaded successfully!~%")
(format t "Run (ch-tests:run-all-tests) to start testing.~%")
(format t "Run (ch-tests:example-usage) for more information.~%")