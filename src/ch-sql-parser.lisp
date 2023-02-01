(defpackage :clickhouse.ch-sql-parser
  (:use #:cl #:lexer #:boost-json)
  (:export :make-query
           :formatter
           :*format*
	   :json-formats))

(in-package :clickhouse.ch-sql-parser)

(defvar *format* nil)

(defun make-query (query)
  "Sets detected format, passes query."
  (auto-formatter query)
  (values query))

(define-lexer ch-lexer (state)
  ("%s+"   (values :next-token))
  (","     (values :comma))
  ("%a%w*" (values :ident $$))
  ("%d+"   (values :int (parse-integer $$))))

(defun syntax-parser (query)
  "Tokenizes a query using ch-lexer."
  (tokenize 'ch-lexer query))

(defun auto-formatter (input)
  (let ((lexer (syntax-parser input))
	(chosen-format))
    (setf *format* nil)
    (loop for i from 0 below (length lexer) and lexeme across (coerce lexer 'vector)
	  do (if (equalp "FORMAT" (token-value lexeme))
		 (progn
		   (setf chosen-format (token-value (nth (+ 1 i) lexer)))
		   (cond ((equal chosen-format "JSON") (setf *format* 'json))
			 (t (setf *format* nil))))))))

(defun json-formats (input)
  (json-decode input))
			  
