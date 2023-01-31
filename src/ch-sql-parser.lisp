(defpackage :clickhouse.ch-sql-parser
  (:use #:cl #:lexer)
  (:import-from :clickhouse.utils
		:coerce-by-length)
  (:export :make-query
           :formatter))

(in-package :clickhouse.ch-sql-parser)
  
(defun make-query (query)
  (formatter query)
  (values query))

(define-lexer ch-lexer (state)
  ("%s+"   (values :next-token))
  ("%a%w*" (values :ident $$))
  ("%d+"   (values :int (parse-integer $$))))

(defun syntax-parser (query)
  (tokenize 'ch-lexer query))

(defun formatter (input)
  (let ((lexer (syntax-parser input))
	(chosen-format))
    (print lexer)
    (loop for i from 0 below (length lexer) and lexeme across (coerce lexer 'vector)
	  do (if (equalp "FORMAT" (token-value lexeme))
		 (progn
		   (setf chosen-format (token-value (nth (+ 1 i) lexer)))
		   (print chosen-format)
		   (cond ((equal chosen-format "JSONEachRow") (setf clickhouse::*format* 'jsoneachrow))
			 (t (setf clickhouse::*format* nil))))))))
			   

