;;;; basic-connection.lisp
;;;; 
;;;; Basic connection and query examples for clickhouse-cl
;;;; This example demonstrates the fundamental operations.

(format t "~%=== ClickHouse-CL Basic Connection Example ===~%")

;; Make sure the library is loaded
(unless (find-package :ch)
  (format t "Loading clickhouse-cl...~%")
  (load (merge-pathnames "ch.lisp" (or *load-pathname* *default-pathname-defaults*))))

;; Configuration - adjust these for your setup
(defparameter *host* "localhost"
  "ClickHouse server hostname")

(defparameter *port* 8123
  "ClickHouse server port")

(defparameter *username* "default"
  "Database username")

(defparameter *password* nil
  "Database password (nil for no password)")

(format t "~%1. Creating database connection...~%")

;; Create database connection
(defparameter *db* (ch:make-database 
                    :host *host*
                    :port *port*
                    :username *username*
                    :password *password*))

(format t "Connected to: ~A~%" *db*)

(format t "~%2. Testing connection with ping...~%")

;; Test the connection
(handler-case
    (let ((ping-result (ch:ping *db*)))
      (format t "Ping successful: ~A~%" ping-result))
  (error (e)
    (format t "Ping failed: ~A~%" e)
    (format t "Make sure ClickHouse is running on ~A:~A~%" *host* *port*)))

(format t "~%3. Basic queries...~%")

;; Simple SELECT query
(format t "~%Simple SELECT:~%")
(let ((result (ch:query *db* "SELECT 1 as number, 'hello' as greeting")))
  (format t "Result: ~A~%" result))

;; Get ClickHouse version
(format t "~%ClickHouse version:~%")
(let ((version (ch:query *db* "SELECT version()")))
  (format t "Version: ~A~%" version))

;; Show databases
(format t "~%Available databases:~%")
(let ((databases (ch:query *db* "SHOW DATABASES")))
  (format t "~A~%" databases))

;; Current time
(format t "~%Current server time:~%")
(let ((time (ch:query *db* "SELECT now() as current_time")))
  (format t "Time: ~A~%" time))

;; Simple calculation
(format t "~%Simple calculation:~%")
(let ((calc (ch:query *db* "SELECT 2 + 2 as result")))
  (format t "2 + 2 = ~A~%" calc))

(format t "~%4. System information queries...~%")

;; System tables query
(format t "~%System tables (first 5):~%")
(let ((tables (ch:query *db* "SELECT name FROM system.tables LIMIT 5")))
  (format t "~A~%" tables))

;; Server uptime
(format t "~%Server uptime:~%")
(let ((uptime (ch:query *db* "SELECT uptime() as uptime_seconds")))
  (format t "Uptime: ~A seconds~%" uptime))

(format t "~%=== Basic Connection Example Complete ===~%")

;; Usage instructions
(format t "~%Next steps:~%")
(format t "- Modify *host*, *port*, *username*, *password* for your setup~%")
(format t "- Try: (ch:query *db* \"YOUR_QUERY_HERE\")~%")
(format t "- Use: (ch:ping *db*) to test connectivity~%")
(format t "- Database object is available as *db*~%")