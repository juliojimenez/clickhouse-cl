;;;; error-handling.lisp
;;;; 
;;;; Comprehensive error handling examples for clickhouse-cl
;;;; Demonstrates proper error management and recovery strategies

(format t "~%=== ClickHouse Error Handling Examples ===~%")

;; Load library
(unless (find-package :ch)
  (load "ch.lisp"))

;; Connection parameters
(defparameter *good-db* (ch:make-database :host "localhost" :port 8123))
(defparameter *bad-db* (ch:make-database :host "nonexistent-host" :port 9999))

(format t "~%=== ERROR TYPE DEMONSTRATIONS ===~%")

;; 1. Connection Errors
(format t "~%1. CONNECTION ERRORS:~%")

(format t "Testing connection to nonexistent host...~%")
(handler-case
    (ch:ping *bad-db*)
  (ch:connection-error (e)
                       (format t "Caught connection error: ~A~%" (ch:clickhouse-error-message e)))
  (error (e)
    (format t "Caught general error: ~A~%" e)))

(format t "~%Testing connection to good host...~%")
(handler-case
    (progn
     (ch:ping *good-db*)
     (format t "Connection successful!~%"))
  (ch:connection-error (e)
                       (format t "Connection failed: ~A~%" (ch:clickhouse-error-message e)))
  (error (e)
    (format t "Unexpected error: ~A~%" e)))

;; 2. Query Errors
(format t "~%2. QUERY ERRORS:~%")

(format t "Testing invalid SQL syntax...~%")
(handler-case
    (ch:query *good-db* "SELCT 1") ; Intentional typo
  (ch:query-error (e)
                  (format t "Query error caught: ~A~%" (ch:clickhouse-error-message e))
                  (format t "Failed query was: ~A~%" (ch:query-error-query e)))
  (error (e)
    (format t "General error: ~A~%" e)))

(format t "~%Testing query on nonexistent table...~%")
(handler-case
    (ch:query *good-db* "SELECT * FROM nonexistent_table")
  (ch:query-error (e)
                  (format t "Query error: ~A~%" (ch:clickhouse-error-message e)))
  (error (e)
    (format t "Error: ~A~%" e)))

;; 3. Error Recovery Patterns
(format t "~%=== ERROR RECOVERY PATTERNS ===~%")

;; Pattern 1: Retry with exponential backoff
(format t "~%1. RETRY PATTERN:~%")

(defun query-with-retry (db query &key (max-retries 3) (initial-delay 1))
  "Execute query with retry logic and exponential backoff"
  (let ((delay initial-delay))
    (dotimes (attempt max-retries)
      (handler-case
          (return-from query-with-retry (ch:query db query))
        (ch:connection-error (e)
                             (format t "Attempt ~A failed: ~A~%" (1+ attempt) (ch:clickhouse-error-message e))
                             (when (< attempt (1- max-retries))
                                   (format t "Retrying in ~A seconds...~%" delay)
                                   (sleep delay)
                                   (setf delay (* delay 2))))
        (ch:query-error (e)
                        ;; Don't retry query errors - they won't succeed
                        (format t "Query error (not retrying): ~A~%" (ch:clickhouse-error-message e))
                        (return-from query-with-retry nil))))
    (format t "All retry attempts failed~%")
    nil))

;; Test retry pattern
(format t "Testing retry pattern with bad connection...~%")
(query-with-retry *bad-db* "SELECT 1" :max-retries 2 :initial-delay 0.5)

;; Pattern 2: Graceful degradation
(format t "~%2. GRACEFUL DEGRADATION PATTERN:~%")

(defun get-user-count (db &optional (fallback-value "Unknown"))
  "Get user count with fallback value on error"
  (handler-case
      (let ((result (ch:query db "SELECT count() as count FROM system.users FORMAT JSON")))
        (if (listp result)
            (ch:jget (first (ch:jget result "data")) "count")
            fallback-value))
    (error (e)
      (format t "Failed to get user count: ~A~%" e)
      fallback-value)))

(format t "User count: ~A~%" (get-user-count *good-db*))

;; Pattern 3: Multiple database fallback
(format t "~%3. MULTIPLE DATABASE FALLBACK:~%")

(defun execute-with-fallback (query primary-db &rest fallback-dbs)
  "Execute query on primary database, fallback to others if needed"
  (let ((all-dbs (cons primary-db fallback-dbs)))
    (dolist (db all-dbs)
      (handler-case
          (return-from execute-with-fallback
                       (values (ch:query db query) db))
        (error (e)
          (format t "Database ~A failed: ~A~%" db e)
          (when (eq db (car (last all-dbs)))
                (format t "All databases failed!~%")))))
    (values nil nil)))

;; Test fallback pattern
(format t "Testing fallback pattern...~%")
(multiple-value-bind (result successful-db)
    (execute-with-fallback "SELECT 1" *bad-db* *good-db*)
  (if result
      (format t "Query succeeded on ~A: ~A~%" successful-db result)
      (format t "Query failed on all databases~%")))

;; 4. Validation and Sanitization
(format t "~%=== VALIDATION AND SANITIZATION ===~%")

(defun safe-table-query (db table-name)
  "Safely query a table with validation - returns (values result success-p error-message)"
  ;; Validate table name (basic sanitization)
  (cond
   ((null table-name)
     (values nil nil "Table name cannot be null"))
   ((zerop (length table-name))
     (values nil nil "Table name cannot be empty"))
   ((find #\; table-name)
     (values nil nil "Table name cannot contain semicolons"))
   ((find #\' table-name)
     (values nil nil "Table name cannot contain single quotes"))
   (t
     ;; Validation passed, try the query
     (handler-case
         (let ((result (ch:query db (format nil "SELECT count() FROM ~A" table-name))))
           (values result t nil))
       (ch:query-error (e)
                       (values nil nil (format nil "Table query failed: ~A" (ch:clickhouse-error-message e))))
       (error (e)
         (values nil nil (format nil "Unexpected error: ~A" e)))))))

;; Test validation
(format t "Testing table validation...~%")

;; Test valid table
(multiple-value-bind (result success-p error-msg)
    (safe-table-query *good-db* "system.tables")
  (if success-p
      (format t "✓ Valid table query succeeded: ~A~%" result)
      (format t "✗ Valid table query failed: ~A~%" error-msg)))

;; Test invalid table  
(multiple-value-bind (result success-p error-msg)
    (safe-table-query *good-db* "bad'table;DROP")
  (if success-p
      (format t "✗ Invalid table query should have failed but got: ~A~%" result)
      (format t "✓ Invalid table query properly rejected: ~A~%" error-msg)))

;; Test empty table name
(multiple-value-bind (result success-p error-msg)
    (safe-table-query *good-db* "")
  (if success-p
      (format t "✗ Empty table name should have failed but got: ~A~%" result)
      (format t "✓ Empty table name properly rejected: ~A~%" error-msg)))

;; Test nil table name
(multiple-value-bind (result success-p error-msg)
    (safe-table-query *good-db* nil)
  (if success-p
      (format t "✗ Nil table name should have failed but got: ~A~%" result)
      (format t "✓ Nil table name properly rejected: ~A~%" error-msg)))

;; 5. Custom Error Handling Wrapper
(format t "~%=== CUSTOM ERROR WRAPPER ===~%")

(defmacro with-clickhouse-error-handling ((&key on-connection-error
                                                on-query-error
                                                on-general-error)
                                          &body body)
  "Macro for standardized error handling"
  `(handler-case
       (progn ,@body)
     (ch:connection-error (e)
                          ,(if on-connection-error
                               `(funcall ,on-connection-error e)
                               `(format t "Connection error: ~A~%" (ch:clickhouse-error-message e))))
     (ch:query-error (e)
                     ,(if on-query-error
                          `(funcall ,on-query-error e)
                          `(format t "Query error: ~A~%" (ch:clickhouse-error-message e))))
     (error (e)
       ,(if on-general-error
            `(funcall ,on-general-error e)
            `(format t "General error: ~A~%" e)))))

;; Test custom wrapper
(format t "Testing custom error wrapper...~%")
(with-clickhouse-error-handling
 (:on-query-error (lambda (e)
                    (format t "Custom query error handler: ~A~%"
                      (ch:clickhouse-error-message e))))
 (ch:query *good-db* "INVALID SQL HERE"))

;; 6. Transaction-style Error Handling
(format t "~%=== TRANSACTION-STYLE OPERATIONS ===~%")

(defun safe-database-operation (db operation-name &rest queries)
  "Execute multiple queries safely, log all operations"
  (format t "Starting operation: ~A~%" operation-name)
  (let ((executed-queries '())
        (success t))
    (dolist (query queries)
      (handler-case
          (progn
           (format t "  Executing: ~A~%" (if (> (length query) 50)
                                             (format nil "~A..." (subseq query 0 50))
                                             query))
           (ch:query db query)
           (push query executed-queries)
           (format t "  ✓ Success~%"))
        (error (e)
          (format t "  ✗ Failed: ~A~%" e)
          (setf success nil)
          (return))))

    (if success
        (format t "Operation '~A' completed successfully (~A queries)~%"
          operation-name (length executed-queries))
        (format t "Operation '~A' failed after ~A successful queries~%"
          operation-name (length executed-queries)))

    success))

;; Test safe operation
(format t "Testing safe database operation...~%")
(safe-database-operation *good-db*
                         "Test Operation"
                         "SELECT 1"
                         "SELECT 2"
                         "INVALID QUERY"
                         "SELECT 3")

;; 7. Error Logging
(format t "~%=== ERROR LOGGING EXAMPLE ===~%")

(defparameter *error-log* '())

(defun log-error (error-type message &optional query)
  "Log error to simple error log"
  (let ((log-entry (list :timestamp (get-universal-time)
                         :type error-type
                         :message message
                         :query query)))
    (push log-entry *error-log*)
    (format t "ERROR LOGGED: ~A - ~A~%" error-type message)))

(defun query-with-logging (db query)
  "Execute query with error logging"
  (handler-case
      (ch:query db query)
    (ch:connection-error (e)
                         (log-error :connection (ch:clickhouse-error-message e) query)
                         nil)
    (ch:query-error (e)
                    (log-error :query (ch:clickhouse-error-message e) (ch:query-error-query e))
                    nil)
    (error (e)
      (log-error :general (format nil "~A" e) query)
      nil)))

;; Test logging
(format t "Testing error logging...~%")
(query-with-logging *good-db* "SELECT 1") ; Should succeed
(query-with-logging *good-db* "INVALID") ; Should fail and log

;; Show error log
(format t "~%Error log contents:~%")
(dolist (entry (reverse *error-log*))
  (format t "  ~A: ~A~%"
    (getf entry :type)
    (getf entry :message)))

;; Best Practices Summary
(format t "~%=== ERROR HANDLING BEST PRACTICES ===~%")
(format t "1. Use specific error types (connection-error, query-error)~%")
(format t "2. Implement retry logic for transient failures~%")
(format t "3. Provide fallback values or alternative data sources~%")
(format t "4. Validate and sanitize inputs before queries~%")
(format t "5. Log errors for debugging and monitoring~%")
(format t "6. Use error handling macros for consistency~%")
(format t "7. Don't retry query errors (syntax errors won't improve)~%")
(format t "8. Clean up resources in error conditions~%")

(format t "~%=== Error Handling Examples Complete ===~%")
