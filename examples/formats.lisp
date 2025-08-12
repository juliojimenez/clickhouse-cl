;;;; formats.lisp
;;;; 
;;;; Demonstration of different ClickHouse output formats
;;;; Shows how clickhouse-cl handles various data formats

(format t "~%=== ClickHouse Formats Example ===~%")

;; Ensure library is loaded
(unless (find-package :ch)
  (load (merge-pathnames "ch.lisp" (or *load-pathname* *default-pathname-defaults*))))

;; Create database connection (adjust as needed)
(defparameter *db* (ch:make-database :host "localhost"))

(format t "Testing connection...~%")
(handler-case
    (ch:ping *db*)
  (error (e)
    (format t "Connection failed: ~A~%" e)
    (format t "Please ensure ClickHouse is running~%")
    (error "ClickHouse connection required for formats example")))

(format t "Connection successful!~%")

;; Sample data query for demonstrations
(defparameter *sample-query*
              "SELECT 
                1 as id,
                'John Doe' as name, 
                25 as age,
                'john@example.com' as email,
                '2024-01-15 10:30:00' as created_at
              UNION ALL SELECT 
                2, 'Jane Smith', 30, 'jane@example.com', '2024-01-16 11:45:00'
              UNION ALL SELECT 
                3, 'Bob Wilson', 22, 'bob@example.com', '2024-01-17 09:15:00'")

(format t "~%=== FORMAT DEMONSTRATIONS ===~%")

;; 1. JSON Format
(format t "~%1. JSON Format:~%")
(format t "Query: ~A FORMAT JSON~%" *sample-query*)
(let ((json-result (ch:query *db* (format nil "~A FORMAT JSON" *sample-query*))))
  (format t "Result type: ~A~%" (type-of json-result))
  (format t "Sample data access:~%")
  (when (listp json-result)
        (let ((data (ch:jget json-result "data")))
          (when data
                (format t "  First row name: ~A~%"
                  (ch:jget (first data) "name"))
                (format t "  Number of rows: ~A~%" (length data)))))
  (format t "Full result: ~A~%" json-result))

;; 2. CSV Format  
(format t "~%2. CSV Format:~%")
(let ((csv-result (ch:query *db* (format nil "~A FORMAT CSV" *sample-query*))))
  (format t "Result type: ~A~%" (type-of csv-result))
  (format t "CSV data:~%")
  (when (listp csv-result)
        (dolist (row csv-result)
          (format t "  ~A~%" row))))

;; 3. TabSeparated Format
(format t "~%3. TabSeparated Format:~%")
(let ((tab-result (ch:query *db* (format nil "~A FORMAT TabSeparated" *sample-query*))))
  (format t "Result type: ~A~%" (type-of tab-result))
  (format t "Tab-separated data:~%")
  (when (listp tab-result)
        (dolist (row tab-result)
          (format t "  ~A~%" row))))

;; 4. Pretty Format
(format t "~%4. Pretty Format (human readable):~%")
(let ((pretty-result (ch:query *db* (format nil "~A FORMAT Pretty" *sample-query*))))
  (format t "Result type: ~A~%" (type-of pretty-result))
  (format t "Pretty output:~%~A~%" pretty-result))

;; 5. Raw query (no format processing)
(format t "~%5. Raw Query (no processing):~%")
(let ((raw-result (ch:query *db* (format nil "~A FORMAT JSON" *sample-query*) :raw t)))
  (format t "Result type: ~A~%" (type-of raw-result))
  (format t "Raw JSON string:~%~A~%" raw-result))

;; 6. Working with JSON data
(format t "~%=== JSON DATA MANIPULATION ===~%")
(let ((json-data (ch:query *db* (format nil "~A FORMAT JSON" *sample-query*))))
  (when (listp json-data)
        (let ((rows (ch:jget json-data "data")))
          (format t "Processing ~A rows:~%" (length rows))
          (dolist (row rows)
            (format t "  User: ~A (age ~A, email ~A)~%"
              (ch:jget row "name")
              (ch:jget row "age")
              (ch:jget row "email"))))))

;; 7. Aggregation with JSON
(format t "~%=== AGGREGATION EXAMPLE ===~%")
(let ((agg-query "SELECT 
                    count() as total_users,
                    avg(age) as avg_age,
                    min(age) as min_age,
                    max(age) as max_age
                  FROM (
                    SELECT 25 as age UNION ALL 
                    SELECT 30 UNION ALL 
                    SELECT 22
                  ) FORMAT JSON"))
  (let ((agg-result (ch:query *db* agg-query)))
    (when (listp agg-result)
          (let ((stats (first (ch:jget agg-result "data"))))
            (format t "Statistics:~%")
            (format t "  Total users: ~A~%" (ch:jget stats "total_users"))
            (format t "  Average age: ~A~%" (ch:jget stats "avg_age"))
            (format t "  Min age: ~A~%" (ch:jget stats "min_age"))
            (format t "  Max age: ~A~%" (ch:jget stats "max_age"))))))

;; 8. Format detection
(format t "~%=== AUTOMATIC FORMAT DETECTION ===~%")
(format t "The library automatically detects FORMAT clause in queries:~%")

;; Query without explicit format parameter - library detects JSON
(let ((auto-json (ch:query *db* "SELECT 'auto-detected' as message FORMAT JSON")))
  (format t "Auto-detected JSON: ~A~%" (type-of auto-json)))

;; Query without explicit format parameter - library detects CSV  
(let ((auto-csv (ch:query *db* "SELECT 'auto-detected' as message FORMAT CSV")))
  (format t "Auto-detected CSV: ~A~%" (type-of auto-csv)))

(format t "~%=== Advanced Format Tips ===~%")
(format t "1. Use JSON for structured data that you want to process~%")
(format t "2. Use CSV for simple tabular data or file exports~%")
(format t "3. Use Pretty formats for human-readable output~%")
(format t "4. Use :raw t parameter to get unprocessed strings~%")
(format t "5. Access JSON data with (ch:jget object \"key\")~%")

(format t "~%=== Formats Example Complete ===~%")
