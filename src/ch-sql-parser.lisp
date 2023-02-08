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
  ("SHOW"        (values :show))
  ("Show"        (values :show))
  ("show"        (values :show))
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
  ("RENAME"      (values :rename))
  ("Rename"      (values :rename))
  ("rename"      (values :rename))
  ("EXCHANGE"    (values :exchange))
  ("Exchange"    (values :exchange))
  ("exchange"    (values :exchange))
  ("JOIN"        (values :join))
  ("Join"        (values :join))
  ("join"        (values :join))
  ("EXPLAIN"     (values :explain))
  ("Explain"     (values :explain))
  ("explain"     (values :explain))
  ("ASC"         (values :asc))
  ("Asc"         (values :asc))
  ("asc"         (values :asc))
  ("DESC"        (values :desc))
  ("Desc"        (values :desc))
  ("desc"        (values :desc))
  ("AS"          (values :as))
  ("As"          (values :as))
  ("as"          (values :as))
  ("IN"          (values :in))
  ("In"          (values :in))
  ("in"          (values :in))
  ("IF"          (values :if))
  ("If"          (values :if))
  ("if"          (values :if))
  ("NOT"         (values :not))
  ("Not"         (values :not))
  ("not"         (values :not))
  ("EXISTS"      (values :exists))
  ("Exists"      (values :exists))
  ("exists"      (values :exists))
  ("FROM"        (values :from))
  ("From"        (values :from))
  ("from"        (values :from))
  ("INTO"        (values :into))
  ("Into"        (values :into))
  ("into"        (values :into))
  ("DATABASE"    (values :database))
  ("Database"    (values :database))
  ("database"    (values :database))
  ("DATABASES"   (values :databases))
  ("Databases"   (values :databases))
  ("databases"   (values :databases))
  ("TABLE"       (values :table))
  ("Table"       (values :table))
  ("table"       (values :table))
  ("TABLES"      (values :tables))
  ("Tables"      (values :tables))
  ("tables"      (values :tables))
  ("WHERE"                  (values :where))
  ("Where"                  (values :where))
  ("where"                  (values :where))
  ("LIKE"                   (values :like))
  ("Like"                   (values :like))
  ("like"                   (values :like))
  ("LIMIT"                  (values :limit))
  ("Limit"                  (values :limit))
  ("limit"                  (values :limit))
  ("FORMAT"                 (values :format))
  ("Format"                 (values :format))
  ("format"                 (values :format))
  ("PRIMARY"                (values :primary))
  ("Primary"                (values :primary))
  ("primary"                (values :primary))
  ("KEY"                    (values :key))
  ("Key"                    (values :key))
  ("key"                    (values :key))
  ("GROUP"                  (values :group))
  ("Group"                  (values :group))
  ("group"                  (values :group))
  ("ORDER"                  (values :order))
  ("Order"                  (values :order))
  ("order"                  (values :order))
  ("BY"                     (values :by))
  ("By"                     (values :by))
  ("by"                     (values :by))
  ("SETTINGS"               (values :settings))
  ("Settings"               (values :settings))
  ("settings"               (values :settings))
  ("ENGINE"                 (values :engine))
  ("Engine"                 (values :engine))
  ("engine"                 (values :engine))
  ("COMMENT"                (values :comment))
  ("Comment"                (values :comment))
  ("comment"                (values :comment))
  ;types
  ("Array"                  (values :array))
  ("Bool"                   (values :bool))
  ("Date"                   (values :date))
  ("DateTime"               (values :datetime))
  ("DateTime64"             (values :datetime64))
  ("Decimal"                (values :decimal))
  ("FixedString"            (values :fixedstring))
  ("Float"                  (values :float))
  ("Float32"                (values :float32))
  ("Int8"                   (values :int8))
  ("Int32"                  (values :int32))
  ("Int64"                  (values :int64))
  ("Nullable"               (values :nullable))
  ("String"                 (values :string))
  ("UInt32"                 (values :uint32))
  ("UInt64"                 (values :uint64))
  ; engines
  ("MergeTree"              (values :mergetree))
  ("Replicated"             (values :replicated))
  ("ReplicatedMergeTree"    (values :replicatedmergetree))
  ("Atomic"                 (values :atomic))
  ("Lazy"                   (values :lazy))
  ("PostgreSQL"             (values :postgresql))
  ("MaterializedPostgreSQL" (values :materializedpostgresql))
  ("MySQL"                  (values :mysql))
  ("MaterializedMySQL"      (values :materializedmysql))
  ("SQLite"                 (values :sqlite))
  ("View"                   (values :view))
  ("MaterializedView"       (values :materializedview))
  ("Dictionary"             (values :dictionary))
  ("SummingMergeTree"       (values :summingmergetree))
  ("Null"                   (values :null))
  ; table functions
  ("file"                   (values :file))
  ("url"                    (values :url))
  ("s3"                     (values :s3))
  ("hdfs"                   (values :hdfs))
  ("mysql"                  (values :mysqlfunc))
  ("postgresql"             (values :postgresqlfunc))
  ("sqlite"                 (values :sqlitefunc))
  ("odbc"                   (values :odbc))
  ("jdbc"                   (values :jdbc))
  ("numbers"                (values :numbers))
  ; special characters
  ("%*"                     (values :wildcard))
  (","                      (values :comma))
  ("%."                     (values :period))
  (":"                      (values :colon))
  (";"                      (values :semicolon))
  ("="                      (values :equal))
  ("!="                     (values :notequal))
  (">"                      (values :greaterthan))
  ("<"                      (values :lessthan))
  (">="                     (values :greaterequalthan))
  ("<="                     (values :lessequalthan))
  ("%+"                     (values :plus))
  ("%-"                     (values :minus))
  ("%_"                     (values :underscore))
  ("%%"                     (values :percent))
  ("/"                      (values :fslash))
  ("\\"                     (values :bslash))
  ("'"                      (values :singleq))
  ("\""                     (values :doubleq))
  ("%("                     (values :lparen))
  ("%)"                     (values :rparen))
  ("%{"                     (values :lcurl))
  ("%}"                     (values :rcurl))
  ("%["                     (values :lbracket))
  ("%]"                     (values :rbracket))
  ; identifiers
  ("%a%w*"                  (values :ident $$))
  ("%a"                     (values :ident $$))
  ; numbers
  ("%d+"                    (values :int (parse-integer $$))))

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
              ((equal chosen-format "Pretty") (setf *format* 'pretty))
              ((equal chosen-format "TabSeparated") (setf *format* 'tabseparated))
              ((equal chosen-format "TabSeparatedRaw") (setf *format* 'tabseparatedraw))
              ((equal chosen-format "TabSeparatedWithNames") (setf *format* 'tabseparatedwithnames))
              ((equal chosen-format "TabSeparatedWithNamesAndTypes") (setf *format* 'tabseparatedwithnamesandtypes))
              ((equal chosen-format "CSV") (setf *format* 'csv))
              (t (setf *format* nil))))))))
			  
(defun to-vector (val)
  (coerce val 'vector))
