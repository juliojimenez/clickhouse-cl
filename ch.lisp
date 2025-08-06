;;;; ch.lisp - ClickHouse Common Lisp Client
;;;; 
;;;; Usage: (load "ch.lisp")
;;;; 
;;;; Example:
;;;;   (defparameter *db* (ch:make-database :host "localhost"))
;;;;   (ch:query *db* "SELECT 1")
;;;;
;;;; Author: julio@clickhouse.com
;;;; License: Apache-2.0
;;;; Version: 0.49.0

(defpackage :clickhouse
  (:nicknames :ch)
  (:use :cl)
  (:export
   ;; Database
   #:database
   #:make-database
   #:ping
   #:query
   #:execute
   #:insert-file

   ;; Utilities
   #:jget
   #:with-connection
   #:format-connection-string

   ;; Conditions
   #:clickhouse-error
   #:connection-error
   #:query-error))

(in-package :clickhouse)

;;;; ============================================================================
;;;; VENDORED DEPENDENCIES
;;;; ============================================================================

;; Load required socket libraries
#+sbcl (require :sb-bsd-sockets)

;;;; Simple JSON Parser (replaces boost-json)
(defun json-parse (string)
  "Parse JSON string into Lisp objects. Minimal implementation."
  (let ((pos 0)
        (len (length string)))

    (labels ((peek-json-char ()
                             (when (< pos len)
                                   (char string pos)))

             (next-json-char ()
                             (prog1 (peek-json-char)
                               (incf pos)))

             (skip-whitespace ()
                              (loop while (and (< pos len)
                                               (member (peek-json-char) '(#\Space #\Tab #\Newline #\Return)))
                                    do (incf pos)))

             (parse-string ()
                           (next-json-char) ; skip opening quote
                           (let ((start pos))
                             (loop while (and (< pos len)
                                              (not (char= (peek-json-char) #\")))
                                   do (if (char= (peek-json-char) #\\)
                                          (progn (incf pos) (incf pos)) ; skip escaped char
                                          (incf pos)))
                             (prog1 (subseq string start pos)
                               (next-json-char)))) ; skip closing quote

             (parse-number ()
                           (let ((start pos))
                             (when (char= (peek-json-char) #\-)
                                   (next-json-char))
                             (loop while (and (< pos len)
                                              (digit-char-p (peek-json-char)))
                                   do (next-json-char))
                             (when (and (< pos len) (char= (peek-json-char) #\.))
                                   (next-json-char)
                                   (loop while (and (< pos len)
                                                    (digit-char-p (peek-json-char)))
                                         do (next-json-char)))
                             (read-from-string (subseq string start pos))))

             (parse-array ()
                          (next-json-char) ; skip [
                          (skip-whitespace)
                          (let ((result '()))
                            (unless (char= (peek-json-char) #\])
                              (loop
                               (push (parse-value) result)
                               (skip-whitespace)
                               (case (peek-json-char)
                                 (#\, (next-json-char) (skip-whitespace))
                                 (#\] (return))
                                 (t (error "Expected ',' or ']' in array")))))
                            (next-json-char) ; skip ]
                            (nreverse result)))

             (parse-object ()
                           (next-json-char) ; skip {
                           (skip-whitespace)
                           (let ((result '()))
                             (unless (char= (peek-json-char) #\})
                               (loop
                                (skip-whitespace)
                                (unless (char= (peek-json-char) #\")
                                  (error "Expected string key in object"))
                                (let ((key (parse-string)))
                                  (skip-whitespace)
                                  (unless (char= (next-json-char) #\:)
                                    (error "Expected ':' after object key"))
                                  (skip-whitespace)
                                  (let ((value (parse-value)))
                                    (push (cons key value) result)))
                                (skip-whitespace)
                                (case (peek-json-char)
                                  (#\, (next-json-char) (skip-whitespace))
                                  (#\} (return))
                                  (t (error "Expected ',' or '}' in object")))))
                             (next-json-char) ; skip }
                             result))

             (parse-literal (literal value)
                            (when (and (<= (+ pos (length literal)) len)
                                       (string= string literal :start1 pos :end1 (+ pos (length literal))))
                                  (incf pos (length literal))
                                  value))

             (parse-value ()
                          (skip-whitespace)
                          (case (peek-json-char)
                            (#\" (parse-string))
                            (#\{ (parse-object))
                            (#\[ (parse-array))
                            (#\t (or (parse-literal "true" t)
                                     (error "Invalid literal")))
                            (#\f (or (parse-literal "false" nil)
                                     (error "Invalid literal")))
                            (#\n (or (parse-literal "null" nil)
                                     (error "Invalid literal")))
                            (otherwise
                             (if (or (digit-char-p (peek-json-char))
                                     (char= (peek-json-char) #\-))
                                 (parse-number)
                                 (error "Unexpected character: ~A" (peek-json-char)))))))

      (parse-value))))

(defun json-get (object key)
  "Get value from JSON object (alist) by key string."
  (cdr (assoc key object :test #'string=)))

;;;; Simple HTTP Client (replaces dexador)

(defun make-http-request (method host port path &key content headers ssl timeout)
  "Make HTTP request. Returns response body as string."
  (declare (ignore ssl timeout)) ; TODO: implement SSL and timeout
  (let ((socket nil)
        (stream nil))
    (unwind-protect
        (progn
         ;; Create socket based on Lisp implementation
         #+sbcl
         (progn
          (setf socket (make-instance 'sb-bsd-sockets:inet-socket
                         :type :stream
                         :protocol :tcp))
          (sb-bsd-sockets:socket-connect socket
            (sb-bsd-sockets:host-ent-address
              (sb-bsd-sockets:get-host-by-name host))
            port)
          (setf stream (sb-bsd-sockets:socket-make-stream socket
                                                          :input t
                                                          :output t
                                                          :element-type 'character)))
         #+ccl
         (setf stream (ccl:make-socket :remote-host host :remote-port port))

         #+ecl
         (setf stream (ext:make-socket-stream host port))

         #+clisp
         (setf stream (socket:socket-connect port host))

         #-(or sbcl ccl ecl clisp)
         (error "HTTP client not implemented for this Lisp implementation")

         ;; Send HTTP request
         (format stream "~A ~A HTTP/1.1~C~C" method path #\Return #\Linefeed)
         (format stream "Host: ~A~C~C" host #\Return #\Linefeed)
         (format stream "Connection: close~C~C" #\Return #\Linefeed)
         (format stream "User-Agent: clickhouse-cl/1.0~C~C" #\Return #\Linefeed)

         ;; Add custom headers
         (dolist (header headers)
           (format stream "~A: ~A~C~C" (car header) (cdr header) #\Return #\Linefeed))

         ;; Add content if present
         (when content
               (let ((content-length (length content)))
                 (format stream "Content-Length: ~D~C~C" content-length #\Return #\Linefeed)
                 (format stream "Content-Type: text/plain; charset=utf-8~C~C" #\Return #\Linefeed)))

         (format stream "~C~C" #\Return #\Linefeed) ; End headers

         ;; Send body
         (when content
               (write-string content stream))

         (force-output stream)

         ;; Read response
         (let ((status-line (read-line stream nil nil)))
           (unless (and status-line (search "200" status-line))
             (error 'connection-error
               :message (format nil "HTTP Error: ~A" (or status-line "No response")))))

         ;; Skip headers
         (loop for line = (read-line stream nil nil)
               while (and line
                          (> (length line) 0)
                          (not (string= (string-trim '(#\Return) line) ""))))

         ;; Read body
         (let ((body (make-string-output-stream)))
           (loop for line = (read-line stream nil nil)
                 while line
                 do (progn
                     (write-string line body)
                     (write-char #\Newline body)))
           (get-output-stream-string body)))

      ;; Cleanup
      (when stream
            (ignore-errors (close stream)))
      #+sbcl
      (when socket
            (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun encode-string-to-octets (string)
  "Convert string to octets for content-length calculation."
  #+sbcl (sb-ext:string-to-octets string :external-format :utf-8)
  #+ccl (ccl:encode-string-to-octets string :external-format :utf-8)
  #+ecl (ext:string-to-octets string :external-format :utf-8)
  #+clisp (ext:convert-string-to-bytes string charset:utf-8)
  #-(or sbcl ccl ecl clisp) (map 'vector #'char-code string))

;;;; Simple Regex Functions (replaces cl-ppcre for basic needs)
(defun regex-match (pattern string)
  "Simple regex matching. Returns match position or NIL."
  (search pattern string :test #'char-equal))

(defun extract-format-from-query (query)
  "Extract FORMAT clause from SQL query using simple string parsing."
  (let ((upper-query (string-upcase query)))
    (let ((format-pos (search "FORMAT " upper-query)))
      (when format-pos
            (let* ((start (+ format-pos 7)) ; length of "FORMAT "
                                           (end (or (position #\Space upper-query :start start)
                                                    (position #\; upper-query :start start)
                                                    (length upper-query))))
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (subseq query start end)))))))

;;;; ============================================================================
;;;; CONDITIONS (ERROR HANDLING)
;;;; ============================================================================

(define-condition clickhouse-error (error)
    ((message :initarg :message :reader clickhouse-error-message))
  (:report (lambda (condition stream)
             (format stream "ClickHouse Error: ~A" (clickhouse-error-message condition)))))

(define-condition connection-error (clickhouse-error)
    ()
  (:report (lambda (condition stream)
             (format stream "ClickHouse Connection Error: ~A" (clickhouse-error-message condition)))))

(define-condition query-error (clickhouse-error)
    ((query :initarg :query :reader query-error-query))
  (:report (lambda (condition stream)
             (format stream "ClickHouse Query Error: ~A~%Query: ~A"
               (clickhouse-error-message condition)
               (query-error-query condition)))))

;;;; ============================================================================
;;;; DATABASE CLASS AND OPERATIONS
;;;; ============================================================================

(defclass database ()
    ((host :initarg :host :initform "localhost" :accessor database-host
           :documentation "ClickHouse server hostname")
     (port :initarg :port :initform 8123 :accessor database-port
           :documentation "ClickHouse server port")
     (ssl :initarg :ssl :initform nil :accessor database-ssl
          :documentation "Use SSL connection")
     (username :initarg :username :initform "default" :accessor database-username
               :documentation "Database username")
     (password :initarg :password :initform nil :accessor database-password
               :documentation "Database password")
     (database :initarg :database :initform "default" :accessor database-name
               :documentation "Database name")
     (timeout :initarg :timeout :initform 60 :accessor database-timeout
              :documentation "Query timeout in seconds"))
  (:documentation "ClickHouse database connection"))

(defun make-database (&key (host "localhost") (port 8123) ssl
                           (username "default") password
                           (database "default") (timeout 60))
  "Create a new database connection object."
  (make-instance 'database
    :host host :port port :ssl ssl
    :username username :password password
    :database database :timeout timeout))

(defmethod print-object ((db database) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~A:~A/~A"
      (database-host db)
      (database-port db)
      (database-name db))))

;;;; ============================================================================
;;;; HTTP INTERFACE
;;;; ============================================================================

(defun build-auth-header (username password)
  "Build HTTP Basic Authentication header."
  (when (and username password)
        (let ((credentials (format nil "~A:~A" username password)))
          (cons "Authorization"
                (format nil "Basic ~A"
                  #+sbcl (sb-ext:octets-to-string
                           (sb-ext:string-to-octets credentials :external-format :utf-8))
                  #-sbcl credentials)))))

(defun make-clickhouse-request (db path &key content (method "GET"))
  "Make HTTP request to ClickHouse server."
  (handler-case
      (let ((headers '()))
        (let ((auth-header (build-auth-header (database-username db) (database-password db))))
          (when auth-header
                (push auth-header headers)))

        (make-http-request method
                           (database-host db)
                           (database-port db)
                           path
                           :content content
                           :headers headers
                           :ssl (database-ssl db)
                           :timeout (database-timeout db)))
    (error (e)
      (error 'connection-error :message (format nil "~A" e)))))

;;;; ============================================================================
;;;; FORMAT HANDLING
;;;; ============================================================================

(defparameter *format-processors*
              '(("JSON" . process-json-format)
                ("JSONEACHROW" . process-json-each-row-format)
                ("JSONCOMPACT" . process-json-format)
                ("CSV" . process-csv-format)
                ("TABSEPARATED" . process-tab-separated-format)
                ("VALUES" . process-values-format)
                ("PRETTY" . identity)
                ("PRETTYCOMPACT" . identity)
                ("TSKV" . process-tskv-format))
              "Format processors for different ClickHouse output formats.")

(defun process-json-format (response)
  "Process JSON format response."
  (handler-case
      (json-parse response)
    (error (e)
      (warn "Failed to parse JSON: ~A" e)
      response)))

(defun split-string (string delimiter)
  "Split string by delimiter character."
  (let ((parts '())
        (start 0))
    (loop for i from 0 below (length string)
            when (char= (char string i) delimiter)
          do (progn
              (push (subseq string start i) parts)
              (setf start (1+ i)))
          finally (push (subseq string start) parts))
    (nreverse parts)))

(defun process-json-each-row-format (response)
  "Process JSONEachRow format - each line is a JSON object."
  (let ((lines (remove-if (lambda (line) (zerop (length (string-trim '(#\Space #\Tab) line))))
                   (split-string response #\Newline))))
    (mapcar (lambda (line)
              (handler-case
                  (json-parse line)
                (error (e)
                  (warn "Failed to parse JSON line: ~A" e)
                  line)))
        lines)))

(defun process-csv-format (response)
  "Process CSV format response."
  (let ((lines (split-string response #\Newline)))
    (mapcar (lambda (line) (split-string line #\,))
        (remove-if (lambda (line) (zerop (length (string-trim '(#\Space #\Tab) line)))) lines))))

(defun process-tab-separated-format (response)
  "Process TabSeparated format response."
  (let ((lines (split-string response #\Newline)))
    (mapcar (lambda (line) (split-string line #\Tab))
        (remove-if (lambda (line) (zerop (length (string-trim '(#\Space #\Tab) line)))) lines))))

(defun process-values-format (response)
  "Process Values format response."
  ;; Simple implementation - split by lines and basic parsing
  (let ((lines (split-string response #\Newline)))
    (remove-if (lambda (line) (zerop (length (string-trim '(#\Space #\Tab) line)))) lines)))

(defun process-tskv-format (response)
  "Process TSKV format response."
  (let ((lines (split-string response #\Newline)))
    (mapcar (lambda (line)
              (let ((pairs (split-string line #\Tab)))
                (mapcar (lambda (pair)
                          (let ((eq-pos (position #\= pair)))
                            (when eq-pos
                                  (cons (subseq pair 0 eq-pos)
                                        (subseq pair (1+ eq-pos))))))
                    pairs)))
        (remove-if (lambda (line) (zerop (length (string-trim '(#\Space #\Tab) line)))) lines))))

(defun process-response (response format)
  "Process response based on detected or specified format."
  (let ((processor (cdr (assoc (string-upcase (or format "")) *format-processors* :test #'string=))))
    (if processor
        (funcall processor response)
        response)))

;;;; ============================================================================
;;;; PUBLIC API
;;;; ============================================================================

(defgeneric ping (database &key)
  (:documentation "Ping the ClickHouse server to test connectivity."))

(defmethod ping ((db database) &key (endpoint "/ping"))
  "Ping ClickHouse server."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (make-clickhouse-request db endpoint)))

(defgeneric query (database query-string &key)
  (:documentation "Execute a query against the ClickHouse database."))

(defmethod query ((db database) query-string &key format timeout raw)
  "Execute query and return processed results."
  (declare (ignore timeout)) ; TODO: implement timeout support
  (handler-case
      (let* ((detected-format (or format (extract-format-from-query query-string)))
             (response (make-clickhouse-request db
                                                "/"
                                                :content query-string
                                                :method "POST")))
        (if raw
            response
            (process-response response detected-format)))
    (error (e)
      (error 'query-error :message (format nil "~A" e) :query query-string))))

(defgeneric execute (database query-string &key)
  (:documentation "Execute a query and return raw response."))

(defmethod execute ((db database) query-string &key)
  "Execute query and return raw response string."
  (query db query-string :raw t))

(defgeneric insert-file (database file-path table-name &key)
  (:documentation "Insert data from file into table."))

(defmethod insert-file ((db database) file-path table-name &key (format "CSV"))
  "Insert file contents into ClickHouse table."
  (with-open-file (stream file-path :direction :input :element-type 'character)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (let ((insert-query (format nil "INSERT INTO ~A FORMAT ~A" table-name format)))
        (make-clickhouse-request db
                                 "/"
                                 :content (format nil "~A~%~A" insert-query content)
                                 :method "POST")))))

;;;; ============================================================================
;;;; UTILITY FUNCTIONS
;;;; ============================================================================

(defun jget (json-object key)
  "Get value from JSON object by key (compatibility with old API)."
  (json-get json-object key))

(defmacro with-connection ((var connection-spec) &body body)
  "Execute body with database connection."
  `(let ((,var (if (typep ,connection-spec 'database)
                   ,connection-spec
                   (apply #'make-database ,connection-spec))))
     ,@body))

(defun format-connection-string (host &key (port 8123) ssl username password database)
  "Format connection parameters into a connection string."
  (format nil "~A://~@[~A~@[:~A~]@~]~A:~D~@[/~A~]"
    (if ssl "https" "http")
    username password
    host port database))

;;;; ============================================================================
;;;; EXAMPLES AND DOCUMENTATION
;;;; ============================================================================

#|
USAGE EXAMPLES:

;; Basic usage
(defparameter *db* (ch:make-database :host "localhost"))
(ch:ping *db*)
(ch:query *db* "SELECT 1")

;; With authentication
(defparameter *db* (ch:make-database 
                    :host "localhost"
                    :username "myuser" 
                    :password "mypass"))

;; Query with format
(ch:query *db* "SELECT name, count() FROM users GROUP BY name FORMAT JSON")

;; Raw query
(ch:execute *db* "SHOW TABLES")

;; Insert file
(ch:insert-file *db* "/path/to/data.csv" "my_table" :format "CSV")

;; Using with-connection macro
(ch:with-connection (db '(:host "localhost" :username "user"))
  (ch:query db "SELECT version()"))

;; JSON access
(let ((result (ch:query *db* "SELECT name, age FROM users FORMAT JSON")))
  (ch:jget result "data"))

|#

(format t " ~%")
(format t "    ██  ██  ██  ██  λ~%")
(format t "    ██  ██  ██  ██~%")
(format t "    ██  ██  ██  ██~%")
(format t "    ██  ██  ██  ██  ██~%")
(format t "    ██  ██  ██  ██  ██~%")
(format t "    ██  ██  ██  ██~%")
(format t "    ██  ██  ██  ██~%")
(format t "    ██  ██  ██  ██~%")
(format t " ~%")
(format t "ClickHouse Common Lisp Client loaded successfully!~%")
(format t "Version: 0.49.0~%")
(format t "Usage: (ch:make-database :host \"localhost\")~% ~%")