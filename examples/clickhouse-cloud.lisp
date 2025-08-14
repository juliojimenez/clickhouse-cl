;;;; clickhouse-cloud.lisp
;;;; 
;;;; Examples for connecting to ClickHouse Cloud
;;;; Demonstrates SSL connections, authentication, and cloud-specific features

(format t "~%=== ClickHouse Cloud Connection Examples ===~%")

;; Load library
(unless (find-package :ch)
  (load (merge-pathnames "ch.lisp" (or *load-pathname* *default-pathname-defaults*))))

(format t "~%=== CLICKHOUSE CLOUD SETUP ===~%")
(format t "This example shows how to connect to ClickHouse Cloud.~%")
(format t "You'll need to update the connection parameters with your actual cloud instance details.~%")

;; ClickHouse Cloud connection parameters
;; IMPORTANT: Replace these with your actual ClickHouse Cloud credentials
(defparameter *cloud-host* "your-instance.clickhouse.cloud"
              "Your ClickHouse Cloud hostname")

(defparameter *cloud-port* 8443
              "ClickHouse Cloud secure port (typically 8443)")

(defparameter *cloud-username* "default"
              "Your ClickHouse Cloud username")

(defparameter *cloud-password* "your-password-here"
              "Your ClickHouse Cloud password")

(defparameter *cloud-database* "default"
              "Database name to connect to")

;; Example with real cloud credentials (commented out for security)
;; Uncomment and modify for actual use:
#|
(defparameter *cloud-host* "abc123def.us-east-1.aws.clickhouse.cloud")
(defparameter *cloud-port* 8443)
(defparameter *cloud-username* "default") 
(defparameter *cloud-password* "SecretPassword123!")
|#

(format t "~%Current configuration:~%")
(format t "  Host: ~A~%" *cloud-host*)
(format t "  Port: ~A~%" *cloud-port*)
(format t "  Username: ~A~%" *cloud-username*)
(format t "  SSL: enabled~%")
(format t "  Database: ~A~%" *cloud-database*)

;; Create ClickHouse Cloud connection
(format t "~%=== CREATING CLOUD CONNECTION ===~%")

(defparameter *cloud-db*
              (ch:make-database :host *cloud-host*
                                :port *cloud-port*
                                :ssl t ; SSL is required for ClickHouse Cloud
                                :username *cloud-username*
                                :password *cloud-password*
                                :database *cloud-database*))

(format t "Cloud database connection object created: ~A~%" *cloud-db*)

;; Test connection with proper error handling
(format t "~%=== TESTING CLOUD CONNECTION ===~%")

(defun test-cloud-connection (db)
  "Test connection to ClickHouse Cloud with detailed feedback"
  (handler-case
      (progn
       (format t "Attempting to ping cloud instance...~%")
       (let ((ping-result (ch:ping db)))
         (format t "✓ Ping successful: ~A~%" ping-result)

         ;; Test basic query
         (format t "Testing basic query...~%")
         (let ((version (ch:query db "SELECT version() as version")))
           (format t "✓ ClickHouse version: ~A~%" version))

         ;; Test server information
         (format t "Getting server information...~%")
         (let ((server-info (ch:query db "SELECT 
                                           timezone() as timezone,
                                           uptime() as uptime_seconds,
                                           currentUser() as current_user
                                         FORMAT JSON")))
           (when (listp server-info)
                 (let ((info (first (ch:jget server-info "data"))))
                   (format t "✓ Server timezone: ~A~%" (ch:jget info "timezone"))
                   (format t "✓ Server uptime: ~A seconds~%" (ch:jget info "uptime_seconds"))
                   (format t "✓ Current user: ~A~%" (ch:jget info "current_user")))))

         t))
    (ch:connection-error (e)
                         (format t "✗ Connection failed: ~A~%" (ch:clickhouse-error-message e))
                         (format t "~%Troubleshooting tips:~%")
                         (format t "1. Verify your hostname is correct~%")
                         (format t "2. Check that you're using port 8443 (secure port)~%")
                         (format t "3. Ensure SSL is enabled~%")
                         (format t "4. Verify your credentials~%")
                         (format t "5. Check if your IP is whitelisted (if IP filtering is enabled)~%")
                         nil)
    (error (e)
      (format t "✗ Unexpected error: ~A~%" e)
      nil)))

;; Run connection test
(if (and (not (string= *cloud-host* "your-instance.clickhouse.cloud"))
         (not (string= *cloud-password* "your-password-here")))
    ;; Real credentials provided
    (test-cloud-connection *cloud-db*)
    ;; Demo mode - show what would happen
    (progn
     (format t "DEMO MODE: Using placeholder credentials~%")
     (format t "To test with real ClickHouse Cloud:~%")
     (format t "1. Replace *cloud-host* with your actual hostname~%")
     (format t "2. Replace *cloud-password* with your actual password~%")
     (format t "3. Re-run this example~%")))

;; Cloud-specific features and examples
(format t "~%=== CLICKHOUSE CLOUD FEATURES ===~%")

(defun demo-cloud-features (db)
  "Demonstrate ClickHouse Cloud specific features (if connected)"

  ;; Check available databases
  (format t "~%1. Available Databases:~%")
  (handler-case
      (let ((databases (ch:query db "SHOW DATABASES")))
        (format t "~A~%" databases))
    (error (e)
      (format t "Could not retrieve databases: ~A~%" e)))

  ;; Show cloud service information
  (format t "~%2. Service Information:~%")
  (handler-case
      (let ((service-info (ch:query db "SELECT 
                                         getSetting('max_memory_usage') as max_memory,
                                         getSetting('max_execution_time') as max_execution_time,
                                         getSetting('max_concurrent_queries') as max_concurrent
                                       FORMAT JSON")))
        (when (listp service-info)
              (let ((info (first (ch:jget service-info "data"))))
                (format t "Max memory usage: ~A~%" (ch:jget info "max_memory"))
                (format t "Max execution time: ~A~%" (ch:jget info "max_execution_time"))
                (format t "Max concurrent queries: ~A~%" (ch:jget info "max_concurrent")))))
    (error (e)
      (format t "Could not retrieve service info: ~A~%" e)))

  ;; Check system metrics (if available)
  (format t "~%3. System Metrics:~%")
  (handler-case
      (let ((metrics (ch:query db "SELECT 
                                   name, 
                                   value 
                                 FROM system.metrics 
                                 WHERE name IN ('Query', 'Merge', 'PartMutation')
                                 FORMAT JSON")))
        (when (listp metrics)
              (let ((data (ch:jget metrics "data")))
                (dolist (metric data)
                  (format t "~A: ~A~%"
                    (ch:jget metric "name")
                    (ch:jget metric "value"))))))
    (error (e)
      (format t "Could not retrieve metrics: ~A~%" e))))

;; Run cloud features demo if we have a real connection
(format t "~%Demonstrating cloud features...~%")
(if (and (not (string= *cloud-host* "your-instance.clickhouse.cloud"))
         (not (string= *cloud-password* "your-password-here")))
    (demo-cloud-features *cloud-db*)
    (format t "DEMO MODE: Would show cloud-specific features with real connection~%"))

;; Connection best practices for cloud
(format t "~%=== CLOUD CONNECTION BEST PRACTICES ===~%")

(defun create-robust-cloud-connection (host username password &key
                                            (port 8443)
                                            (database "default")
                                            (timeout 30))
  "Create a robust ClickHouse Cloud connection with validation"

  ;; Validate parameters
  (unless (and host (> (length host) 0))
    (error "Host is required"))

  (unless (and username (> (length username) 0))
    (error "Username is required"))

  (unless (and password (> (length password) 0))
    (error "Password is required"))

  ;; Validate cloud hostname format
  (unless (search "clickhouse.cloud" host)
    (warn "Host doesn't appear to be a ClickHouse Cloud instance"))

  ;; Create connection with cloud-optimized settings
  (let ((db (ch:make-database :host host
                              :port port
                              :ssl t ; Always use SSL for cloud
                              :username username
                              :password password
                              :database database
                              :timeout timeout)))

    ;; Test connection immediately
    (handler-case
        (ch:ping db)
      (error (e)
        (error "Failed to connect to ClickHouse Cloud: ~A" e)))

    (format t "✓ Successfully connected to ClickHouse Cloud~%")
    db))

;; Example of robust connection creation
(format t "~%Example of robust connection creation:~%")
(format t "(create-robust-cloud-connection \"your-host.clickhouse.cloud\" \"user\" \"pass\")~%")

;; Security considerations
(format t "~%=== SECURITY CONSIDERATIONS ===~%")
(format t "1. ALWAYS use SSL (port 8443) for ClickHouse Cloud~%")
(format t "2. Use strong passwords and consider rotating them regularly~%")
(format t "3. Enable IP filtering in cloud console if possible~%")
(format t "4. Use environment variables for credentials, not hardcoded values~%")
(format t "5. Monitor connection attempts in the cloud console~%")
(format t "6. Use read-only users for applications that only query data~%")

;; Environment variable example
(format t "~%=== ENVIRONMENT VARIABLE EXAMPLE ===~%")
(format t "Instead of hardcoding credentials, use environment variables:~%")
(format t "~%")
(format t "(defparameter *cloud-host* (or (sb-ext:posix-getenv \"CLICKHOUSE_HOST\")~%")
(format t "                              \"localhost\"))~%")
(format t "(defparameter *cloud-password* (sb-ext:posix-getenv \"CLICKHOUSE_PASSWORD\"))~%")
(format t "~%")
(format t "Then set environment variables:~%")
(format t "export CLICKHOUSE_HOST=your-instance.clickhouse.cloud~%")
(format t "export CLICKHOUSE_PASSWORD=your-secure-password~%")

;; Sample query for cloud analytics
(format t "~%=== SAMPLE CLOUD ANALYTICS QUERIES ===~%")

(defparameter *sample-cloud-queries*
              '("-- Get current connections
     SELECT * FROM system.processes FORMAT Pretty"

                "-- Check query performance
     SELECT 
       query_duration_ms,
       query,
       user,
       client_name
     FROM system.query_log 
     WHERE event_time > now() - INTERVAL 1 HOUR
     ORDER BY query_duration_ms DESC
     LIMIT 10
     FORMAT Pretty"

                "-- Database sizes
     SELECT 
       database,
       sum(bytes) as size_bytes,
       sum(rows) as total_rows
     FROM system.parts
     WHERE active = 1
     GROUP BY database
     ORDER BY size_bytes DESC
     FORMAT Pretty"))

(format t "Sample queries you can run on ClickHouse Cloud:~%")
(dolist (query *sample-cloud-queries*)
  (format t "~%~A~%" query))

(format t "~%=== GETTING YOUR CLOUD CREDENTIALS ===~%")
(format t "1. Go to https://clickhouse.cloud/~%")
(format t "2. Sign in to your account~%")
(format t "3. Create a new service or select existing one~%")
(format t "4. Get connection details from the 'Connect' tab~%")
(format t "5. Use the provided hostname, port 8443, and your credentials~%")

(format t "~%=== ClickHouse Cloud Examples Complete ===~%")
(format t "Remember to replace placeholder credentials with your actual cloud instance details!~%")
