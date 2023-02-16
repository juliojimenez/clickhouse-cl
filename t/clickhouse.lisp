(defpackage :clickhouse-test
  (:use :cl :fiveam)
  (:import-from :clickhouse.utils
                :csv-formatter
                :format-url
                :ver)
  (:export #:all-tests
	   #:run!))

(in-package :clickhouse-test)

(defparameter *db-test* nil)

(defvar *format* nil)

(def-suite all-tests :description "all tests suite.")

(in-suite all-tests)

; clickhouse.utils

(test csv-formatter-simple
  (is (equalp 'cons (type-of (csv-formatter "c1,c2,c3")))))

(test format-url-non-ssl
  (is (string= "http://localhost:8123" (format-url "localhost" 8123 nil ""))))

(test format-url-ssl
  (is (string= "https://localhost:8123" (format-url "localhost" 8123 t ""))))

(test format-url-ssl-uri
  (is (string= "https://localhost:8443/ping" (format-url "localhost" 8443 t "/ping"))))

(test json-formats
  (is (string=
       "pass"
       (clickhouse:jget (clickhouse.utils::json-formats "{ \"testing\": \"pass\" }") "testing"))))

(test prettify-console
  (is (equalp nil (clickhouse.utils:prettify "1." :console t))))

(test prettify-nil
  (is (string= "1." (clickhouse.utils:prettify "1." :console nil))))

(test pretty-formatter
  (is (string= "┌───┐
│ 1 │
├───┤
│ 1 │
└───┘
" (clickhouse.utils::pretty-formatter "┏━━━┓
┃ [1m1[0m ┃
┡━━━┩
│ 1 │
└───┘"))))

(test pretty-formatter-clean-input
  (is (equalp 'cons (type-of (clickhouse.utils::pretty-formatter-clean-input "┏━━━┓
┃ [1m1[0m ┃
┡━━━┩
│ 1 │
└───┘")))))

(test pretty-formatter-positions
  (is (equalp '(0 4) (clickhouse.utils::pretty-formatter-positions "│ 1 │"))))

(test pretty-formatter-title-row-split
  (is (equalp '("1") (clickhouse.utils::pretty-formatter-title-row-split "┃ 1 ┃"))))

(test pretty-formatter-title-row
  (is (equalp "│ 1 │
" (clickhouse.utils::pretty-formatter-title-row '(0 4) '("1")))))

(test pretty-formatter-border-top
  (is (equalp "┌───┐
" (clickhouse.utils::pretty-formatter-border 
    '(0 4)
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
    #\BOX_DRAWINGS_LIGHT_HORIZONTAL))))

(test pretty-formatter-border-bottom
  (is (equalp "├───┤
" (clickhouse.utils::pretty-formatter-border 
    '(0 4)
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL
    #\BOX_DRAWINGS_LIGHT_HORIZONTAL))))

(test tab-separated-formatter
  (is (equalp
       '("7" "8" "9")
       (first
	(clickhouse.utils::tab-separated-formatter "c1	c2	c3
1	2	3
4	5	6
7	8	9")))))

(test ver-nil
  (is (equalp nil (ver nil))))

(test ver-t
  (is (equalp t (ver "okie dokie"))))

; clickhouse.ch-sql-parser

(test auto-formatter
  (is (equalp *format* (clickhouse.ch-sql-parser::auto-formatter "SELECT 1 FORMAT JSONEachRow"))))

(test make-query
  (is (string= "SELECT 1" (clickhouse.ch-sql-parser:make-query "SELECT 1"))))

(test syntax-parser
  (is (equalp "SELECT" (lexer:token-lexeme (first (clickhouse.ch-sql-parser::syntax-parser "SELECT 1"))))))

; database class and methods

(test make-database
  (finishes (defparameter *db-test* (make-instance 'clickhouse:database))))

(test ping-implicit
  (is (string= "Ok." (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test*)))))

(test ping-explicit
  (is (string= "Ok." (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test* :ping t)))))

(test ping-implicit-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test* :console t)))))

(test ping-explicit-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:ping *db-test* :ping t :console t)))))

(test replicas-status
  (is (string= "Ok." (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			    (clickhouse:replicas-status *db-test*)))))

(test replica-status-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			 (clickhouse:replicas-status *db-test* :console t)))))

(test easy-query
  (is (string= "1" (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			  (clickhouse:query *db-test* "SELECT 1")))))

(test easy-query-console
  (is (equalp nil (progn (defparameter *db-test* (make-instance 'clickhouse:database))
			 (clickhouse:query *db-test* "SELECT 1" :console t)))))
