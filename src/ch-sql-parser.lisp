(defpackage :clickhouse.ch-sql-parser
  (:use #:cl #:lexer #:boost-json)
  (:export :make-query
           :formatter
           :*format*))

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
  ("%s+"         (values :next-token))
  ; keywords
  ("SELECT"      (values :select))
  ("Select"      (values :select))
  ("select"      (values :select))
  ("CREATE"      (values :create))
  ("Create"      (values :create))
  ("create"      (values :create))
  ("INSERT"      (values :insert))
  ("Insert"      (values :insert))
  ("insert"      (values :insert))
  ("DROP"        (values :drop))
  ("Drop"        (values :drop))
  ("drop"        (values :drop))
  ("JOIN"        (values :join))
  ("Join"        (values :join))
  ("join"        (values :join))
  ("EXPLAIN"     (values :explain))
  ("Explain"     (values :explain))
  ("explain"     (values :explain))
  ("ASC"         (values :asc))
  ("Asc"         (values :asc))
  ("asc"         (values :asc)
  ("DESC"        (values :desc))
  ("Desc"        (values :desc))
  ("desc"        (values :desc))
  ("AS"          (values :as))
  ("As"          (values :as))
  ("as"          (values :as))
  ("IN"          (values :in))
  ("In"          (values :in))
  ("in"          (values :in))
  ("FROM"        (values :from))
  ("From"        (values :from))
  ("from"        (values :from))
  ("INTO"        (values :into))
  ("Into"        (values :into))
  ("into"        (values :into))
  ("DATABASE"    (values :database))
  ("Database"    (values :database))
  ("database"    (values :database))
  ("TABLE"       (values :table))
  ("Table"       (values :table))
  ("table"       (values :table))
  ("WHERE"       (values :where))
  ("Where"       (values :where))
  ("where"       (values :where))
  ("LIKE"        (values :like))
  ("Like"        (values :like))
  ("like"        (values :like))
  ("LIMIT"       (values :limit))
  ("Limit"       (values :limit))
  ("limit"       (values :limit))
  ("FORMAT"      (values :format))
  ("Format"      (values :format))
  ("format"      (values :format))
  ("PRIMARY"     (values :primary))
  ("Primary"     (values :primary))
  ("primary"     (values :primary))
  ("KEY"         (values :key))
  ("Key"         (values :key))
  ("key"         (values :key))
  ("GROUP"       (values :group))
  ("Group"       (values :group))
  ("group"       (values :group))
  ("ORDER"       (values :order))
  ("Order"       (values :order))
  ("order"       (values :order))
  ("BY"          (values :by))
  ("By"          (values :by))
  ("by"          (values :by))
  ("SETTINGS"    (values :settings))
  ("Settings"    (values :settings))
  ("settings"    (values :settings))
  ("ENGINE"      (values :engine))
  ("Engine"      (values :engine))
  ("engine"      (values :engine))
  ;types
  ("Date"        (values :date))
  ("DateTime"    (values :datetime))
  ("UInt32"      (values :uint32))
  ("FixedString" (values :fixedstring))
  ("Float32"     (values :float32))
  ("String"      (values :string))
  ; engines
  ("MergeTree"   (values :mergetree))
  ; special characters
  ("%*"          (values :wildcard))
  (","           (values :comma))
  ("%."          (values :period))
  (":"           (values :colon))
  (";"           (values :semicolon))
  ("="           (values :equal))
  ("!="          (values :notequal))
  ("%+"          (values :plus))
  ("%-"          (values :minus))
  ("%%"          (values :percent))
  ("/"           (values :fslash))
  ("\\"          (values :bslash))
  ("'"           (values :singleq))
  ("\""          (values :doubleq))
  ("%("          (values :lparen))
  ("%)"          (values :rparen))
  ("%{"          (values :lcurl))
  ("%}"          (values :rcurl))
  ("%["          (values :lbracket))
  ("%]"          (values :rbracket))
  ; identifiers
  ("%a%w*"       (values :ident $$))
  ("%a"          (values :ident $$))
  ; numbers
  ("%d+"         (values :int (parse-integer $$))))

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
			 ((equal chosen-format "TabSeparated") (setf *format* 'tabseparated))
			 (t (setf *format* nil))))))))
			  
(defun to-vector (val)
  (coerce val 'vector))
