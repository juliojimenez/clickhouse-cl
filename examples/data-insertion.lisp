;;;; data-insertion.lisp
;;;; 
;;;; Examples of inserting data into ClickHouse using various methods
;;;; Demonstrates different insertion patterns and formats

(format t "~%=== ClickHouse Data Insertion Examples ===~%")

;; Load library if needed
(unless (find-package :ch)
  (load (merge-pathnames "ch.lisp" (or *load-pathname* *default-pathname-defaults*))))

;; Create connection
(defparameter *db* (ch:make-database :host "localhost"))

;; Test connection
(handler-case
    (ch:ping *db*)
  (error (e)
    (format t "Connection failed: ~A~%" e)
    (format t "Please ensure ClickHouse is running~%")
    (error "ClickHouse connection required for formats example")))

(format t "Connected successfully!~%")

;; Create a test database and table
(format t "~%=== SETTING UP TEST ENVIRONMENT ===~%")

;; Create database
(format t "Creating test database...~%")
(handler-case
    (ch:query *db* "CREATE DATABASE IF NOT EXISTS test_examples")
  (error (e)
    (format t "Database creation note: ~A~%" e)))

;; Switch to test database
(setf *db* (ch:make-database :host "localhost" :database "test_examples"))

;; Create test table
(format t "Creating test table...~%")
(ch:query *db* "DROP TABLE IF EXISTS users")
(ch:query *db* "CREATE TABLE users (
    id UInt32,
    name String,
    email String, 
    age UInt8,
    balance Decimal(10,2),
    created_at DateTime,
    is_active Bool
) ENGINE = MergeTree() 
ORDER BY id")

(format t "Test table 'users' created successfully!~%")

;; Utility Functions
(defun sql-escape-string (str)
  "Basic SQL string escaping - replace single quotes with double quotes"
  (let ((result (make-string (* 2 (length str)) :initial-element #\Space))
        (result-pos 0))
    (loop for char across str do
            (if (char= char #\')
                (progn
                 (setf (char result result-pos) #\')
                 (setf (char result (1+ result-pos)) #\')
                 (incf result-pos 2))
                (progn
                 (setf (char result result-pos) char)
                 (incf result-pos))))
    (subseq result 0 result-pos)))

(defun sql-format-value (value)
  "Format a Lisp value for SQL insertion"
  (cond
   ((null value) "NULL")
   ((eq value t) "1") ; true -> 1
   ((eq value nil) "0") ; false -> 0  
   ((numberp value) (format nil "~A" value))
   ((stringp value) (format nil "'~A'" (sql-escape-string value)))
   (t (format nil "'~A'" (sql-escape-string (format nil "~A" value))))))

;; Method 1: Single row insertion using VALUES
(format t "~%=== METHOD 1: Single Row Insertion ===~%")
(let ((insert-query "INSERT INTO users VALUES 
                       (1, 'John Doe', 'john@example.com', 25, 1000.50, '2024-01-15 10:30:00', true)"))
  (format t "Inserting single row...~%")
  (ch:query *db* insert-query)
  (format t "Row inserted successfully!~%"))

;; Method 2: Multiple rows using VALUES
(format t "~%=== METHOD 2: Multiple Row Insertion ===~%")
(let ((multi-insert "INSERT INTO users VALUES 
                       (2, 'Jane Smith', 'jane@example.com', 30, 2500.75, '2024-01-16 11:45:00', true),
                       (3, 'Bob Wilson', 'bob@example.com', 22, 750.25, '2024-01-17 09:15:00', false),
                       (4, 'Alice Johnson', 'alice@example.com', 28, 3200.00, '2024-01-18 14:20:00', true)"))
  (format t "Inserting multiple rows...~%")
  (ch:query *db* multi-insert)
  (format t "Multiple rows inserted successfully!~%"))

;; Method 3: Programmatic insertion with dynamic data
(format t "~%=== METHOD 3: Programmatic Insertion ===~%")
(defparameter *sample-users*
              '((5 "Charlie Brown" "charlie@example.com" 35 1800.50 "2024-01-19 16:30:00" t)
                (6 "Diana Prince" "diana@example.com" 29 4500.75 "2024-01-20 08:45:00" t)
                (7 "Eve Adams" "eve@example.com" 24 950.25 "2024-01-21 12:15:00" nil)))

(format t "Inserting users programmatically...~%")
(dolist (user *sample-users*)
  (destructuring-bind (id name email age balance created active) user
    (let ((insert-stmt (format nil "INSERT INTO users VALUES (~A, ~A, ~A, ~A, ~A, ~A, ~A)"
                         (sql-format-value id)
                         (sql-format-value name)
                         (sql-format-value email)
                         (sql-format-value age)
                         (sql-format-value balance)
                         (sql-format-value created)
                         (sql-format-value active))))
      (format t "  Inserting: ~A~%" name)
      ;; Uncomment next line for debugging SQL
      ;; (format t "  SQL: ~A~%" insert-stmt)
      (ch:query *db* insert-stmt))))

(format t "Programmatic insertion complete!~%")

;; Method 4: Using SELECT ... UNION ALL pattern
(format t "~%=== METHOD 4: SELECT UNION Pattern ===~%")
(let ((union-insert "INSERT INTO users 
                       SELECT * FROM (
                         SELECT 8, 'Frank Miller', 'frank@example.com', 40, 5500.00, '2024-01-22 10:00:00', true
                         UNION ALL
                         SELECT 9, 'Grace Lee', 'grace@example.com', 27, 2200.50, '2024-01-23 15:30:00', true
                         UNION ALL  
                         SELECT 10, 'Henry Ford', 'henry@example.com', 33, 3700.25, '2024-01-24 09:45:00', false
                       )"))
  (format t "Inserting using SELECT UNION pattern...~%")
  (ch:query *db* union-insert)
  (format t "UNION insertion complete!~%"))

;; Verify insertions
(format t "~%=== VERIFYING INSERTIONS ===~%")
(let ((count-result (ch:query *db* "SELECT count() as total_users FROM users FORMAT JSON")))
  (when (listp count-result)
        (let ((total (ch:jget (first (ch:jget count-result "data")) "total_users")))
          (format t "Total users in table: ~A~%" total))))

;; Show sample data
(format t "~%Sample of inserted data:~%")
(let ((sample-data (ch:query *db* "SELECT id, name, email, age, balance 
                                     FROM users 
                                     ORDER BY id 
                                     LIMIT 5 
                                     FORMAT JSON")))
  (when (listp sample-data)
        (let ((rows (ch:jget sample-data "data")))
          (dolist (row rows)
            (format t "  ID: ~A, Name: ~A, Email: ~A, Age: ~A, Balance: $~A~%"
              (ch:jget row "id")
              (ch:jget row "name")
              (ch:jget row "email")
              (ch:jget row "age")
              (ch:jget row "balance"))))))

;; Method 5: Batch insertion function
(format t "~%=== METHOD 5: Batch Insertion Helper ===~%")

(defun batch-insert-users (db user-data)
  "Helper function to batch insert user data"
  (let ((values-list '()))
    (dolist (user user-data)
      (destructuring-bind (id name email age balance created active) user
        (push (format nil "(~A, ~A, ~A, ~A, ~A, ~A, ~A)"
                (sql-format-value id)
                (sql-format-value name)
                (sql-format-value email)
                (sql-format-value age)
                (sql-format-value balance)
                (sql-format-value created)
                (sql-format-value active))
              values-list)))
    (let ((insert-query (format nil "INSERT INTO users VALUES ~{~A~^,~}"
                          (nreverse values-list))))
      (ch:query db insert-query))))

;; Use the batch function
(let ((batch-users '((11 "Ivy Thompson" "ivy@example.com" 26 1400.75 "2024-01-25 11:20:00" t)
                     (12 "Jack Wilson" "jack@example.com" 31 2800.50 "2024-01-26 14:15:00" t)
                     (13 "Kelly Davis" "kelly@example.com" 23 1200.00 "2024-01-27 16:45:00" nil))))
  (format t "Using batch insertion helper...~%")
  (batch-insert-users *db* batch-users)
  (format t "Batch insertion complete!~%"))

;; Method 6: Error handling during insertion
(format t "~%=== METHOD 6: Error Handling ===~%")
(format t "Attempting invalid insertion to show error handling...~%")
(handler-case
    (ch:query *db* "INSERT INTO users VALUES (1, 'Duplicate ID', 'dupe@example.com', 25, 1000, '2024-01-28', truff)")
  (error (e)
    (format t "Expected error caught: ~A~%" e)
    (format t "This demonstrates proper error handling during insertion~%")))

;; Final verification
(format t "~%=== FINAL VERIFICATION ===~%")
(let ((final-count (ch:query *db* "SELECT count() as total FROM users FORMAT JSON")))
  (when (listp final-count)
        (format t "Final user count: ~A~%"
          (ch:jget (first (ch:jget final-count "data")) "total"))))

;; Show statistics
(let ((stats (ch:query *db* "SELECT 
                              count() as total_users,
                              avg(age) as avg_age,
                              sum(balance) as total_balance,
                              count(case when is_active then 1 end) as active_users
                            FROM users 
                            FORMAT JSON")))
  (when (listp stats)
        (let ((data (first (ch:jget stats "data"))))
          (format t "~%Statistics:~%")
          (format t "  Total users: ~A~%" (ch:jget data "total_users"))
          (format t "  Average age: ~A~%" (ch:jget data "avg_age"))
          (format t "  Total balance: $~A~%" (ch:jget data "total_balance"))
          (format t "  Active users: ~A~%" (ch:jget data "active_users")))))

;; Cleanup reminder
(format t "~%=== CLEANUP ===~%")
(format t "To clean up test data, run:~%")
(format t "  (ch:query *db* \"DROP DATABASE test_examples\")~%")

(format t "~%=== Data Insertion Examples Complete ===~%")

(format t "~%Key takeaways:~%")
(format t "1. Use VALUES format for direct insertions~%")
(format t "2. Use SELECT UNION for complex data transformations~%")
(format t "3. Batch multiple rows in single INSERT for better performance~%")
(format t "4. Always handle errors during data insertion~%")
(format t "5. Use helper functions for repetitive insertion patterns~%")
