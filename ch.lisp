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

