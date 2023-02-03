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
  ;(print query)
  (auto-formatter
   (substitute #\space #\newline query))
  (values query))

(define-lexer ch-lexer (state)
  ; next-token
  ("%s+"    (values :next-token))
  ; keywords
  ("SELECT" (values :select))
  ("Select" (values :select))
  ("select" (values :select))
  ("FROM"   (values :from))
  ("From"   (values :from))
  ("from"   (values :from))
  ("WHERE"  (values :where))
  ("Where"  (values :where))
  ("where"  (values :where))
  ("LIKE"   (values :like))
  ("Like"   (values :like))
  ("like"   (values :like))
  ("LIMIT"  (values :limit))
  ("Limit"  (values :limit))
  ("limit"  (values :limit))
  ("FORMAT" (values :format))
  ("Format" (values :format))
  ("format" (values :format))
  ; special characters
  ("%*"     (values :wildcard))
  (","      (values :comma))
  ("%."     (values :period))
  ("%%"     (values :percent))
  ("'"      (values :singleq))
  ("\""     (values :doubleq))
  ; identifiers
  ("%a%w*"  (values :ident $$))
  ; numbers
  ("%d+"    (values :int (parse-integer $$))))

(defun syntax-parser (query)
  "Tokenizes a query using ch-lexer."
  (tokenize 'ch-lexer query))

(defun auto-formatter (input)
  "Gets FORMAT used and sets it for clickhouse.utils:prettify."
  (let ((lexer (syntax-parser input))
	(chosen-format))
    ;(print lexer)
    (setf *format* nil)
    (loop for i from 0 below (length lexer) and lexeme across (to-vector lexer)
	  do (if (equalp "FORMAT" (token-lexeme lexeme))
		 (progn
		   (setf chosen-format (token-value (nth (+ 1 i) lexer)))
		   ;(print chosen-format)
		   (cond ((equal chosen-format "JSON") (setf *format* 'json))
			 (t (setf *format* nil))))))))

(defun json-formats (input)
  "Decodes input into a BOOST-JSON:JSON-OBJECT."
  (json-decode input))
			  
(defun to-vector (val)
  (coerce val 'vector))
