;;;; performance.lisp
;;;; 
;;;; Performance optimization examples for clickhouse-cl
;;;; Demonstrates efficient query patterns, connection management, and best practices

(format t "~%=== ClickHouse Performance Optimization Examples ===~%")

;; Load library and create connection
(unless (find-package :ch)
  (load (merge-pathnames "ch.lisp" (or *load-pathname* *default-pathname-defaults*))))

(defparameter *db* (ch:make-database :host "localhost"))

;; Utility function for timing operations
(defun time-operation (name operation)
  "Time an operation and return result with timing info"
  (let ((start-time (get-internal-real-time)))
    (multiple-value-prog1 
        (funcall operation)
      (let ((duration (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (format t "~A took ~,3F seconds~%" name duration)))))

;; Test connection
(handler-case
    (ch:ping *db*)
  (error (e)
    (format t "Connection failed: ~A~%" e)
    (error "ClickHouse connection required for performance example")))

(format t "Connection successful!~%")

;; Setup test environment
(format t "~%=== SETTING UP PERFORMANCE TEST ENVIRONMENT ===~%")

;; Create test database
(handler-case
    (ch:query *db* "CREATE DATABASE IF NOT EXISTS perf_test")
  (error (e) 
    (format t "Database creation note: ~A~%" e)))

(setf *db* (ch:make-database :host "localhost" :database "perf_test"))

;; Create test tables with different configurations
(format t "Creating performance test tables...~%")

(ch:query *db* "DROP TABLE IF EXISTS fast_table")
(ch:query *db* "CREATE TABLE fast_table (
    id UInt64,
    timestamp DateTime,
    user_id UInt32,
    event_type String,
    value Float64,
    properties String
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(timestamp)
ORDER BY (user_id, timestamp)
SETTINGS index_granularity = 8192")

(ch:query *db* "DROP TABLE IF EXISTS slow_table")
(ch:query *db* "CREATE TABLE slow_table (
    id UInt64,
    timestamp DateTime,
    user_id UInt32,
    event_type String,
    value Float64,
    properties String
) ENGINE = MergeTree()
ORDER BY id  -- Poor ordering key
SETTINGS index_granularity = 8192")

(format t "Test tables created!~%")

;; Insert test data
(format t "~%=== INSERTING TEST DATA ===~%")

(defun generate-batch-insert (table-name batch-size)
  "Generate a batch insert statement"
  (let ((values '()))
    (dotimes (i batch-size)
      (push (format nil "(~A, '~A', ~A, '~A', ~A, '~A')"
                   i
                   (format nil "2024-01-~2,'0D 10:~2,'0D:00" 
                          (1+ (mod i 28))
                          (mod i 60))
                   (mod i 1000)
                   (nth (mod i 4) '("click" "view" "purchase" "signup"))
                   (* (random 100.0) 1.5)
                   (format nil "{\"category\":\"~A\"}" (mod i 5)))
            values))
    (format nil "INSERT INTO ~A VALUES ~{~A~^,~}" table-name values)))

;; Insert in batches for better performance
(format t "Inserting test data in batches...~%")
(time-operation "Batch insertion (10,000 rows)"
                (lambda ()
                  (dotimes (batch 10)
                    (let ((insert-query (generate-batch-insert "fast_table" 1000)))
                      (ch:query *db* insert-query)))
                  (let ((insert-query (generate-batch-insert "slow_table" 5000)))
                    (ch:query *db* insert-query))))

;; Verify data insertion
(let ((count (ch:query *db* "SELECT count() FROM fast_table FORMAT JSON")))
  (when (listp count)
    (format t "Inserted ~A rows into fast_table~%" 
            (ch:jget (first (ch:jget count "data")) "count()"))))

(format t "~%=== QUERY PERFORMANCE COMPARISONS ===~%")

;; 1. Index Usage Comparison
(format t "~%1. INDEX USAGE COMPARISON:~%")

(format t "~%Good query (uses primary key):~%")
(time-operation "Indexed query"
                (lambda ()
                  (ch:query *db* "SELECT count() 
                                  FROM fast_table 
                                  WHERE user_id = 100 
                                    AND timestamp >= '2024-01-01'
                                    AND timestamp < '2024-01-15'")))

(format t "~%Poor query (doesn't use index efficiently):~%")
(time-operation "Non-indexed query"
                (lambda ()
                  (ch:query *db* "SELECT count() 
                                  FROM slow_table 
                                  WHERE event_type = 'click' 
                                    AND value > 50.0")))

;; 2. Projection vs Full Scan
(format t "~%2. PROJECTION OPTIMIZATION:~%")

(format t "~%Full column scan:~%")
(time-operation "Full scan"
                (lambda ()
                  (ch:query *db* "SELECT * FROM fast_table LIMIT 100")))

(format t "~%Projected columns only:~%")
(time-operation "Column projection"
                (lambda ()
                  (ch:query *db* "SELECT user_id, event_type, value 
                                  FROM fast_table 
                                  LIMIT 100")))

;; 3. Aggregation Performance
(format t "~%3. AGGREGATION PERFORMANCE:~%")

(format t "~%Simple aggregation:~%")
(time-operation "Simple aggregation"
                (lambda ()
                  (ch:query *db* "SELECT 
                                    event_type,
                                    count() as events,
                                    avg(value) as avg_value
                                  FROM fast_table
                                  GROUP BY event_type")))

(format t "~%Complex aggregation:~%")
(time-operation "Complex aggregation"
                (lambda ()
                  (ch:query *db* "SELECT 
                                    toYYYYMM(timestamp) as month,
                                    event_type,
                                    count() as events,
                                    avg(value) as avg_value,
                                    quantile(0.95)(value) as p95_value,
                                    uniq(user_id) as unique_users
                                  FROM fast_table
                                  GROUP BY month, event_type
                                  ORDER BY month, event_type")))

;; 4. Format Performance
(format t "~%4. OUTPUT FORMAT PERFORMANCE:~%")

(defparameter *format-query* "SELECT user_id, event_type, value 
                              FROM fast_table 
                              LIMIT 1000")

(format t "~%JSON format:~%")
(time-operation "JSON format"
                (lambda ()
                  (ch:query *db* (format nil "~A FORMAT JSON" *format-query*))))

(format t "~%CSV format:~%")
(time-operation "CSV format"
                (lambda ()
                  (ch:query *db* (format nil "~A FORMAT CSV" *format-query*))))

(format t "~%TabSeparated format:~%")
(time-operation "TabSeparated format"
                (lambda ()
                  (ch:query *db* (format nil "~A FORMAT TabSeparated" *format-query*))))

(format t "~%Raw format (no processing):~%")
(time-operation "Raw format"
                (lambda ()
                  (ch:query *db* (format nil "~A FORMAT JSON" *format-query*) :raw t)))

;; 5. Connection Management
(format t "~%=== CONNECTION MANAGEMENT ===~%")

(format t "~%5. CONNECTION REUSE vs CREATION:~%")

;; Reusing existing connection
(format t "~%Reusing connection:~%")
(time-operation "Connection reuse"
                (lambda ()
                  (dotimes (i 5)
                    (ch:query *db* "SELECT 1"))))

;; Creating new connections (not recommended)
(format t "~%Creating new connections each time:~%")
(time-operation "New connections"
                (lambda ()
                  (dotimes (i 5)
                    (let ((temp-db (ch:make-database :host "localhost" :database "perf_test")))
                      (ch:query temp-db "SELECT 1")))))

;; 6. Batch Operations
(format t "~%=== BATCH OPERATIONS ===~%")

(format t "~%6. BATCH vs INDIVIDUAL OPERATIONS:~%")

;; Individual inserts (slow)
(ch:query *db* "DROP TABLE IF EXISTS batch_test")
(ch:query *db* "CREATE TABLE batch_test (id UInt32, value String) ENGINE = Memory")

(format t "~%Individual inserts:~%")
(time-operation "Individual inserts"
                (lambda ()
                  (dotimes (i 100)
                    (ch:query *db* (format nil "INSERT INTO batch_test VALUES (~A, 'value~A')" i i)))))

;; Batch insert (fast)
(ch:query *db* "TRUNCATE TABLE batch_test")
(format t "~%Batch insert:~%")
(time-operation "Batch insert"
                (lambda ()
                  (let ((values '()))
                    (dotimes (i 100)
                      (push (format nil "(~A, 'value~A')" i i) values))
                    (ch:query *db* (format nil "INSERT INTO batch_test VALUES ~{~A~^,~}" values)))))

;; 7. Query Optimization Tips
(format t "~%=== QUERY OPTIMIZATION DEMONSTRATIONS ===~%")

(format t "~%7. PREWHERE vs WHERE:~%")

;; Using WHERE (processes all columns)
(format t "~%Using WHERE clause:~%")
(time-operation "WHERE clause"
                (lambda ()
                  (ch:query *db* "SELECT user_id, event_type 
                                  FROM fast_table 
                                  WHERE value > 75.0
                                  LIMIT 100")))

;; Using PREWHERE (more efficient for MergeTree)
(format t "~%Using PREWHERE clause:~%")
(time-operation "PREWHERE clause"
                (lambda ()
                  (ch:query *db* "SELECT user_id, event_type 
                                  FROM fast_table 
                                  PREWHERE value > 75.0
                                  LIMIT 100")))

;; 8. Memory Usage Optimization
(format t "~%8. MEMORY USAGE OPTIMIZATION:~%")

(format t "~%Large result set without LIMIT:~%")
(time-operation "Unlimited query"
                (lambda ()
                  (let ((result (ch:query *db* "SELECT * FROM fast_table FORMAT JSON")))
                    (when (listp result)
                      (format t "Retrieved ~A rows~%" 
                              (length (ch:jget result "data")))))))

(format t "~%Same query with LIMIT:~%")
(time-operation "Limited query"
                (lambda ()
                  (let ((result (ch:query *db* "SELECT * FROM fast_table LIMIT 100 FORMAT JSON")))
                    (when (listp result)
                      (format t "Retrieved ~A rows~%" 
                              (length (ch:jget result "data")))))))

;; 9. Performance Monitoring
(format t "~%=== PERFORMANCE MONITORING ===~%")

(defun get-query-stats (db query)
  "Get performance statistics for a query"
  (let ((stats-query (format nil "~A FORMAT JSON SETTINGS log_queries=1" query)))
    (let ((start-time (get-internal-real-time)))
      (let ((result (ch:query db stats-query)))
        (let ((duration (/ (- (get-internal-real-time) start-time)
                          internal-time-units-per-second)))
          (values result duration))))))

(format t "~%Query with performance monitoring:~%")
(multiple-value-bind (result duration)
    (get-query-stats *db* "SELECT count(*) FROM fast_table WHERE user_id < 100")
  (format t "Query completed in ~,3F seconds~%" duration)
  (format t "Result: ~A~%" result))

;; Performance helper functions
(format t "~%=== PERFORMANCE HELPER FUNCTIONS ===~%")

(defun benchmark-query (db query &key (iterations 5) (format-type "JSON"))
  "Benchmark a query over multiple iterations"
  (format t "Benchmarking: ~A~%" 
          (if (> (length query) 60)
              (format nil "~A..." (subseq query 0 60))
              query))
  
  (let ((times '()))
    (dotimes (i iterations)
      (let ((start (get-internal-real-time)))
        (ch:query db (format nil "~A FORMAT ~A" query format-type))
        (push (/ (- (get-internal-real-time) start)
                 internal-time-units-per-second)
              times)))
    
    (let* ((avg-time (/ (reduce #'+ times) iterations))
           (min-time (apply #'min times))
           (max-time (apply #'max times)))
      (format t "  Average: ~,3Fs, Min: ~,3Fs, Max: ~,3Fs~%" 
              avg-time min-time max-time)
      avg-time)))

;; Test benchmark function
(format t "~%Testing benchmark function:~%")
(benchmark-query *db* "SELECT count() FROM fast_table" :iterations 3)

(defun analyze-table-performance (db table-name)
  "Analyze table performance characteristics"
  (format t "~%Analyzing table: ~A~%" table-name)
  
  ;; Table size and row count
  (let ((size-info (ch:query db (format nil "SELECT 
                                           count() as row_count,
                                           sum(data_compressed_bytes) as compressed_size,
                                           sum(data_uncompressed_bytes) as uncompressed_size
                                         FROM system.parts 
                                         WHERE table = '~A' AND active = 1
                                         FORMAT JSON" table-name))))
    (when (listp size-info)
      (let ((data (first (ch:jget size-info "data"))))
        (format t "  Rows: ~A~%" (ch:jget data "row_count"))
        (format t "  Compressed size: ~A bytes~%" (ch:jget data "compressed_size"))
        (format t "  Uncompressed size: ~A bytes~%" (ch:jget data "uncompressed_size")))))
  
  ;; Index effectiveness test
  (format t "  Testing index effectiveness...~%")
  (benchmark-query db (format nil "SELECT count() FROM ~A WHERE user_id = 1" table-name) 
                   :iterations 2))

;; Analyze our test tables
(analyze-table-performance *db* "fast_table")
(analyze-table-performance *db* "slow_table")

;; Best practices demonstration
(format t "~%=== PERFORMANCE BEST PRACTICES ===~%")

;; Connection pooling simulation
(defparameter *connection-pool* '())

(defun get-pooled-connection ()
  "Simulate connection pooling"
  (if *connection-pool*
      (pop *connection-pool*)
      (ch:make-database :host "localhost" :database "perf_test")))

(defun return-pooled-connection (db)
  "Return connection to pool"
  (push db *connection-pool*))

(defmacro with-pooled-connection ((var) &body body)
  "Use a pooled connection"
  `(let ((,var (get-pooled-connection)))
     (unwind-protect
         (progn ,@body)
       (return-pooled-connection ,var))))

(format t "~%Connection pooling example:~%")
(time-operation "Pooled connections"
                (lambda ()
                  (dotimes (i 3)
                    (with-pooled-connection (conn)
                      (ch:query conn "SELECT 1")))))

;; Efficient data processing patterns
(format t "~%=== EFFICIENT DATA PROCESSING PATTERNS ===~%")

(defun process-large-dataset-streaming (db table-name batch-size)
  "Process large dataset in batches (streaming approach)"
  (format t "Processing ~A in batches of ~A...~%" table-name batch-size)
  (let ((offset 0)
        (processed 0))
    (loop
      (let ((batch (ch:query db (format nil "SELECT id, user_id, value 
                                           FROM ~A 
                                           ORDER BY id 
                                           LIMIT ~A OFFSET ~A 
                                           FORMAT JSON" 
                                       table-name batch-size offset))))
        (if (and (listp batch)
                 (> (length (ch:jget batch "data")) 0))
            (progn
              (let ((rows (ch:jget batch "data")))
                (incf processed (length rows))
                ;; Process each row
                (dolist (row rows)
                  ;; Simulate processing
                  (let ((user-id (ch:jget row "user_id"))
                        (value (ch:jget row "value")))
                    ;; Do something with the data
                    (declare (ignore user-id value)))))
              (incf offset batch-size))
            (return))))
    (format t "Processed ~A rows in batches~%" processed)))

;; Test streaming approach
(format t "~%Streaming data processing:~%")
(time-operation "Streaming processing"
                (lambda ()
                  (process-large-dataset-streaming *db* "fast_table" 500)))

(defun check-system-capabilities (db)
  "Check what system monitoring capabilities are available"
  (format t "Checking ClickHouse monitoring capabilities...~%")
  
  (let ((capabilities '()))
    
    ;; Check basic connectivity
    (handler-case
        (progn
          (ch:query db "SELECT 1")
          (push "Basic queries" capabilities))
      (error () nil))
    
    ;; Check system.processes
    (handler-case
        (progn
          (ch:query db "SELECT 1 FROM system.processes LIMIT 1")
          (push "Process monitoring (system.processes)" capabilities))
      (error () nil))
    
    ;; Check system.query_log
    (handler-case
        (progn
          (ch:query db "SELECT 1 FROM system.query_log LIMIT 1")
          (push "Query logging (system.query_log)" capabilities))
      (error () nil))
    
    ;; Check system.metrics
    (handler-case
        (progn
          (ch:query db "SELECT 1 FROM system.metrics LIMIT 1")
          (push "System metrics (system.metrics)" capabilities))
      (error () nil))
    
    ;; Check system.events
    (handler-case
        (progn
          (ch:query db "SELECT 1 FROM system.events LIMIT 1")
          (push "Event counters (system.events)" capabilities))
      (error () nil))
    
    (if capabilities
        (progn
          (format t "✓ Available capabilities:~%")
          (dolist (cap (reverse capabilities))
            (format t "  • ~A~%" cap)))
        (format t "✗ Limited system access - basic queries only~%"))
    
    ;; Provide guidance
    (format t "~%Monitoring recommendations:~%")
    (unless (member "Query logging (system.query_log)" capabilities :test #'string=)
      (format t "  • Enable query logging: SET log_queries = 1~%"))
    (unless (member "Process monitoring (system.processes)" capabilities :test #'string=)
      (format t "  • Check user permissions for system tables~%"))
    (format t "  • Use basic queries like SELECT version(), uptime()~%")
    
    capabilities))

;; Performance monitoring query  
(format t "~%=== SYSTEM PERFORMANCE MONITORING ===~%")

;; First check what capabilities are available
(check-system-capabilities *db*)

(format t "~%Now attempting performance monitoring:~%")

(defun show-system-performance (db)
  "Show current system performance metrics with graceful fallbacks"
  (format t "Current system performance:~%")
  
  ;; Test basic connectivity first
  (handler-case
      (let ((version (ch:query db "SELECT version() FORMAT JSON")))
        (when (listp version)
          (format t "  ClickHouse version: ~A~%" 
                 (ch:jget (first (ch:jget version "data")) "version()"))))
    (error (e) 
      (format t "  Could not get version: ~A~%" e)
      (return-from show-system-performance)))
  
  ;; Try to get active queries (might not be available)
  (handler-case
      (let ((queries (ch:query db "SELECT 
                                     query_id,
                                     user,
                                     elapsed as query_duration_ms,
                                     formatReadableSize(memory_usage) as memory_used
                                   FROM system.processes 
                                   WHERE query != '' AND query NOT LIKE '%system.processes%'
                                   FORMAT JSON")))
        (when (and (listp queries) (> (length (ch:jget queries "data")) 0))
          (format t "  Active queries: ~A~%" (length (ch:jget queries "data")))
          (dolist (query (ch:jget queries "data"))
            (format t "    User: ~A, Duration: ~As, Memory: ~A~%" 
                   (ch:jget query "user")
                   (/ (ch:jget query "query_duration_ms") 1000.0)
                   (ch:jget query "memory_used"))))
        (when (or (not (listp queries)) (zerop (length (ch:jget queries "data"))))
          (format t "  No active queries currently running~%")))
    (error (e) 
      (format t "  Active queries: Not available (~A)~%" 
             (if (search "404" (format nil "~A" e))
                 "system.processes not accessible"
                 "permission denied or feature disabled"))))
  
  ;; Try alternative memory query
  (handler-case
      (let ((memory (ch:query db "SELECT 
                                    formatReadableSize(total_memory_usage) as total_memory
                                  FROM (SELECT sum(memory_usage) as total_memory_usage FROM system.processes)
                                  FORMAT JSON")))
        (when (listp memory)
          (let ((mem-data (first (ch:jget memory "data"))))
            (format t "  Total memory usage: ~A~%" 
                   (or (ch:jget mem-data "total_memory") "0.00 B")))))
    (error (e) 
      (format t "  Memory usage: Not available: ~A~%" e)))

  ;; Try query log (often disabled by default)
  (handler-case
      (let ((recent (ch:query db "SELECT 
                                    count() as recent_queries,
                                    avg(query_duration_ms) as avg_duration,
                                    max(query_duration_ms) as max_duration
                                  FROM system.query_log 
                                  WHERE event_time > now() - INTERVAL 5 MINUTE
                                    AND type = 2  -- QueryFinish
                                  FORMAT JSON")))
        (when (listp recent)
          (let ((perf-data (first (ch:jget recent "data"))))
            (when (and perf-data (> (ch:jget perf-data "recent_queries") 0))
              (format t "  Recent queries (5min): ~A~%" 
                     (ch:jget perf-data "recent_queries"))
              (format t "  Avg query time (5min): ~,2Fms~%" 
                     (ch:jget perf-data "avg_duration"))
              (format t "  Max query time (5min): ~,2Fms~%" 
                     (ch:jget perf-data "max_duration"))))))
    (error (e) 
      (format t "  Query log: Not available (query logging may be disabled): ~A~%" e)))

  ;; Basic system info that should always work
  (handler-case
      (let ((uptime (ch:query db "SELECT uptime() as uptime_seconds FORMAT JSON")))
        (when (listp uptime)
          (let ((uptime-secs (ch:jget (first (ch:jget uptime "data")) "uptime_seconds")))
            (format t "  Server uptime: ~,1F hours~%" (/ uptime-secs 3600.0)))))
    (error (e) 
      (format t "  Uptime: Not available: ~A~%" e)))

  ;; Show available system tables
  (handler-case
      (let ((sys-tables (ch:query db "SELECT name FROM system.tables 
                                      WHERE database = 'system' 
                                      AND name IN ('processes', 'query_log', 'metrics')
                                      ORDER BY name FORMAT JSON")))
        (when (listp sys-tables)
          (let ((tables (mapcar (lambda (row) (ch:jget row "name")) 
                               (ch:jget sys-tables "data"))))
            (format t "  Available system tables: ~{~A~^, ~}~%" tables)
            (when (not (member "query_log" tables :test #'string=))
              (format t "    Note: query_log not available - enable with log_queries=1~%")))))
    (error (e) 
      (format t "  System tables: Could not enumerate: ~A~%" e))))

(show-system-performance *db*)

;; Performance optimization summary
(format t "~%=== PERFORMANCE OPTIMIZATION SUMMARY ===~%")
(format t "1. CONNECTION MANAGEMENT:~%")
(format t "   - Reuse connections instead of creating new ones~%")
(format t "   - Consider connection pooling for high-traffic applications~%")
(format t "   - Set appropriate timeouts~%")

(format t "~%2. QUERY OPTIMIZATION:~%")
(format t "   - Use appropriate ORDER BY keys for your queries~%")
(format t "   - Use PREWHERE instead of WHERE for MergeTree tables~%")
(format t "   - Project only needed columns, avoid SELECT *~%")
(format t "   - Use LIMIT when you don't need all results~%")
(format t "   - Leverage partitioning for time-series data~%")

(format t "~%3. DATA OPERATIONS:~%")
(format t "   - Use batch inserts instead of individual INSERTs~%")
(format t "   - Choose appropriate table engines~%")
(format t "   - Use compression for large datasets~%")

(format t "~%4. FORMAT SELECTION:~%")
(format t "   - Use TabSeparated for fastest raw data transfer~%")
(format t "   - Use JSON for structured data processing~%")
(format t "   - Use :raw t when you don't need data processing~%")

(format t "~%5. MONITORING:~%")
(format t "   - Monitor query performance with system.query_log~%")
(format t "   - Watch memory usage with system.processes~%")
(format t "   - Use EXPLAIN PLAN to understand query execution~%")

;; Cleanup
(format t "~%=== CLEANUP ===~%")
(format t "To clean up test data:~%")
(format t "  (ch:query *db* \"DROP DATABASE perf_test\")~%")

(format t "~%=== Performance Optimization Examples Complete ===~%")
(format t "Key performance insights:~%")
(format t "- Batch operations are significantly faster than individual ones~%")
(format t "- Proper indexing and query structure matter enormously~%")
(format t "- Connection reuse provides measurable performance benefits~%")
(format t "- Format choice affects both speed and memory usage~%")
(format t "- Monitoring tools help identify bottlenecks~%")
