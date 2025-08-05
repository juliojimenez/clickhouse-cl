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
