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
  ("%s+"                    (values :next-token))
  ; keywords
  ("ALTER"                  (values :alter))
  ("Alter"                  (values :alter))
  ("alter"                  (values :alter))
  ("AS"                     (values :as))
  ("As"                     (values :as))
  ("as"                     (values :as))
  ("ASC"                    (values :asc))
  ("Asc"                    (values :asc))
  ("asc"                    (values :asc))
  ("BY"                     (values :by))
  ("By"                     (values :by))
  ("by"                     (values :by))
  ("CLUSTER"                (values :cluster))
  ("Cluster"                (values :cluster))
  ("cluster"                (values :cluster))
  ("COMMENT"                (values :comment))
  ("Comment"                (values :comment))
  ("comment"                (values :comment))
  ("CREATE"                 (values :create))
  ("Create"                 (values :create))
  ("create"                 (values :create))
  ("DATABASE"               (values :database))
  ("Database"               (values :database))
  ("database"               (values :database))
  ("DATABASES"              (values :databases))
  ("Databases"              (values :databases))
  ("databases"              (values :databases))
  ("DELETE"                 (values :delete))
  ("Delete"                 (values :delete))
  ("delete"                 (values :delete))
  ("DESC"                   (values :desc))
  ("Desc"                   (values :desc))
  ("desc"                   (values :desc))
  ("DETACH"                 (values :detach))
  ("Detach"                 (values :detach))
  ("detach"                 (values :detach))
  ("DROP"                   (values :drop))
  ("Drop"                   (values :drop))
  ("drop"                   (values :drop))
  ("ENGINE"                 (values :engine))
  ("Engine"                 (values :engine))
  ("engine"                 (values :engine))
  ("EXCHANGE"               (values :exchange))
  ("Exchange"               (values :exchange))
  ("exchange"               (values :exchange))
  ("EXISTS"                 (values :exists))
  ("Exists"                 (values :exists))
  ("exists"                 (values :exists))
  ("EXPLAIN"                (values :explain))
  ("Explain"                (values :explain))
  ("explain"                (values :explain))
  ("FORMAT"                 (values :format))
  ("Format"                 (values :format))
  ("format"                 (values :format))
  ("FREEZE"                 (values :freeze))
  ("Freeze"                 (values :freeze))
  ("freeze"                 (values :freeze))
  ("FROM"                   (values :from))
  ("From"                   (values :from))
  ("from"                   (values :from))
  ("GROUP"                  (values :group))
  ("Group"                  (values :group))
  ("group"                  (values :group))
  ("IF"                     (values :if))
  ("If"                     (values :if))
  ("if"                     (values :if))
  ("IN"                     (values :in))
  ("In"                     (values :in))
  ("in"                     (values :in))
  ("INSERT"                 (values :insert))
  ("Insert"                 (values :insert))
  ("insert"                 (values :insert))
  ("INTO"                   (values :into))
  ("Into"                   (values :into))
  ("into"                   (values :into))
  ("JOIN"                   (values :join))
  ("Join"                   (values :join))
  ("join"                   (values :join))
  ("KEY"                    (values :key))
  ("Key"                    (values :key))
  ("key"                    (values :key))
  ("LIKE"                   (values :like))
  ("Like"                   (values :like))
  ("like"                   (values :like))
  ("LIMIT"                  (values :limit))
  ("Limit"                  (values :limit))
  ("limit"                  (values :limit))
  ("MOVE"                   (values :move))
  ("Move"                   (values :move))
  ("move"                   (values :move))
  ("NOT"                    (values :not))
  ("Not"                    (values :not))
  ("not"                    (values :not))
  ("ON"                     (values :on))
  ("On"                     (values :on))
  ("on"                     (values :on))
  ("ORDER"                  (values :order))
  ("Order"                  (values :order))
  ("order"                  (values :order))
  ("PARTITION"              (values :partition))
  ("Partition"              (values :partition))
  ("partition"              (values :partition))
  ("PRIMARY"                (values :primary))
  ("Primary"                (values :primary))
  ("primary"                (values :primary))
  ("RENAME"                 (values :rename))
  ("Rename"                 (values :rename))
  ("rename"                 (values :rename))
  ("REPLACE"                (values :replace))
  ("Replace"                (values :replace))
  ("replace"                (values :replace))
  ("SELECT"                 (values :select))
  ("Select"                 (values :select))
  ("select"                 (values :select))
  ("SETTINGS"               (values :settings))
  ("Settings"               (values :settings))
  ("settings"               (values :settings))
  ("SHOW"                   (values :show))
  ("Show"                   (values :show))
  ("show"                   (values :show))
  ("TABLE"                  (values :table))
  ("Table"                  (values :table))
  ("table"                  (values :table))
  ("TABLES"                 (values :tables))
  ("Tables"                 (values :tables))
  ("tables"                 (values :tables))
  ("TO"                     (values :to))
  ("To"                     (values :to))
  ("to"                     (values :to))
  ("UNION"                  (values :union))
  ("Union"                  (values :union))
  ("union"                  (values :union))
  ("UPDATE"                 (values :update))
  ("Update"                 (values :update))
  ("update"                 (values :update))
  ("WHERE"                  (values :where))
  ("Where"                  (values :where))
  ("where"                  (values :where))
  ;types
  ("Array"                  (values :array))
  ("Bool"                   (values :bool))
  ("Date"                   (values :date))
  ("Date32"                 (values :date32))
  ("DateTime"               (values :datetime))
  ("DateTime64"             (values :datetime64))
  ("Decimal"                (values :decimal))
  ("Decimal32"              (values :decimal32))
  ("Decimal64"              (values :decimal64))
  ("Decimal128"             (values :decimal128))
  ("Decimal256"             (values :decimal256))
  ("Enum"                   (values :enum))
  ("FixedString"            (values :fixedstring))
  ("Float"                  (values :float))
  ("Float32"                (values :float32))
  ("Float64"                (values :float64))
  ("Int8"                   (values :int8))
  ("Int16"                  (values :int16))
  ("Int32"                  (values :int32))
  ("Int64"                  (values :int64))
  ("Int128"                 (values :int128))
  ("Int256"                 (values :int256))
  ("LowCardinality"         (values :lowcardinality))
  ("Nullable"               (values :nullable))
  ("String"                 (values :string))
  ("UInt8"                  (values :uint8))
  ("UInt16"                 (values :uint16))
  ("UInt32"                 (values :uint32))
  ("UInt64"                 (values :uint64))
  ("UInt128"                (values :uint128))
  ("UInt256"                (values :uint256))
  ; engines
  ("Atomic"                 (values :atomic))
  ("Dictionary"             (values :dictionary))
  ("Lazy"                   (values :lazy))
  ("MaterializedPostgreSQL" (values :materializedpostgresql))
  ("MaterializedMySQL"      (values :materializedmysql))
  ("MaterializedView"       (values :materializedview))
  ("MergeTree"              (values :mergetree))
  ("MySQL"                  (values :mysql))
  ("Null"                   (values :null))
  ("PostgreSQL"             (values :postgresql))
  ("Replicated"             (values :replicated))
  ("ReplicatedMergeTree"    (values :replicatedmergetree))
  ("SummingMergeTree"       (values :summingmergetree))
  ("SQLite"                 (values :sqlite))
  ("View"                   (values :view))
  ; functions
  ("avg"                    (values :avg))
  ("randomPrintableASCII"   (values :randomprintableascii))
  ("toDecimal128"           (values :todecimal128))
  ; table functions
  ("file"                   (values :file))
  ("hdfs"                   (values :hdfs))
  ("jdbc"                   (values :jdbc))
  ("mysql"                  (values :mysqlfunc))
  ("numbers"                (values :numbers))
  ("odbc"                   (values :odbc))
  ("postgresql"             (values :postgresqlfunc))
  ("remote"                 (values :remote))
  ("remoteSecure"           (values :remoteSecure))
  ("s3"                     (values :s3))
  ("s3Cluster"              (values :s3cluster))
  ("sqlite"                 (values :sqlitefunc))
  ("url"                    (values :url))
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
              ((equal chosen-format "JSONStrings") (setf *format* 'jsonstrings))
              ((equal chosen-format "JSONColumns") (setf *format* 'jsoncolumns))
              ((equal chosen-format "JSONColumnsWithMetadata") (setf *format* 'jsoncolumnswithmetadata))
              ((equal chosen-format "JSONCompact") (setf *format* 'jsoncompact))
              ((equal chosen-format "JSONCompactStrings") (setf *format* 'jsoncompactstrings))
              ((equal chosen-format "JSONCompactColumns") (setf *format* 'jsoncompactcolumns))
              ((equal chosen-format "JSONEachRow") (setf *format* 'jsoneachrow))
              ((equal chosen-format "JSONEachRowWithProgress") (setf *format* 'jsoneachrowwithprogress))
              ((equal chosen-format "Pretty") (setf *format* 'pretty))
              ((equal chosen-format "TabSeparated") (setf *format* 'tabseparated))
              ((equal chosen-format "TabSeparatedRaw") (setf *format* 'tabseparatedraw))
              ((equal chosen-format "TabSeparatedWithNames") (setf *format* 'tabseparatedwithnames))
              ((equal chosen-format "TabSeparatedWithNamesAndTypes") (setf *format* 'tabseparatedwithnamesandtypes))
              ((equal chosen-format "CSV") (setf *format* 'csv))
              ((equal chosen-format "CSVWithNames") (setf *format* 'csvwithnames))
              ((equal chosen-format "CSVWithNamesAndTypes") (setf *format* 'csvwithnamesandtypes))
              (t (setf *format* nil))))))))
			  
(defun to-vector (val)
  "Coerce a list to a vector."
  (coerce val 'vector))
