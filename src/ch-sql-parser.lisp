(defpackage :clickhouse.ch-sql-parser
  (:use #:cl #:lexer)
  (:import-from :clickhouse.utils
                :coerce-by-length)
  (:export :make-query
           :formatter
           :*format*))

(in-package :clickhouse.ch-sql-parser)

(defparameter *format* nil)

(defun make-query (query)
  (auto-formatter query)
  (values query))

(define-lexer ch-lexer (state)
  ("%s+"   (values :next-token))
  ("%a%w*" (values :ident $$))
  ("%d+"   (values :int (parse-integer $$))))

(defun syntax-parser (query)
  (tokenize 'ch-lexer query))

(defun auto-formatter (input)
  (let ((lexer (syntax-parser input))
	(chosen-format))
    (print lexer)
    (loop for i from 0 below (length lexer) and lexeme across (coerce lexer 'vector)
	  do (if (equalp "FORMAT" (token-value lexeme))
		 (progn
		   (setf chosen-format (token-value (nth (+ 1 i) lexer)))
		   (print chosen-format)
		   (cond ((equal chosen-format "JSONEachRow") (setf *format* 'jsoneachrow))
			 (t (setf *format* nil))))
		 (setf *format* nil)))))
			   

