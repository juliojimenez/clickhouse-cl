(defpackage :clickhouse.utils
  (:use :cl :cl-ppcre)
  (:export :format-url
           :prettify
           :ver))

(in-package :clickhouse.utils)

(defun csv-formatter (input)
	"Process CSV format into a list of lists."
	(let ((csv))
		(dolist (x (uiop:split-string input :separator '(#\Comma)))
			(push (uiop:split-string x :separator '(#\Comma)) csv))
		(values csv)))

(defun format-url (host-slot port-slot ssl-slot uri)
  "Formats a URL"
  (cond (ssl-slot (format nil "https://~a:~a~a" host-slot port-slot uri))
	((not ssl-slot) (format nil "http://~a:~a~a" host-slot port-slot uri))
	(t (format nil "https://~a:~a~a" host-slot port-slot uri))))

(defun json-formats (input)
  "Decodes input into a BOOST-JSON:JSON-OBJECT."
  (boost-json:json-decode input))

(defun prettify (body &key console formatting)
  "Output conditioner and formatter resolver."
  (terpri)
  (let ((b (string-trim '(#\Newline) body)))
    (cond ((and (ver console) (ver formatting) (equalp formatting clickhouse.ch-sql-parser::'pretty))
	   			 (format t "~d" (pretty-formatter b)))
					((ver formatting) (cond ((equalp formatting clickhouse.ch-sql-parser::'json)
																	(json-formats b))
																	((equalp formatting clickhouse.ch-sql-parser::'pretty)
																	(pretty-formatter b))
																	((or
																		(equalp formatting clickhouse.ch-sql-parser::'tabseparated)
																		(equalp formatting clickhouse.ch-sql-parser::'tabseparatedraw)
																		(equalp formatting clickhouse.ch-sql-parser::'tabseparatedwithnames)
																		(equalp formatting clickhouse.ch-sql-parser::'tabseparatedwithnamesandtypes))
																	(tab-separated-formatter b))
																	((equalp formatting clickhouse.ch-sql-parser::'csv)
																	(csv-formatter b))))
	  			(console (format t "~d" b))
	  			(t (values b)))))

(defun pretty-formatter (input)
  "Clean up Pretty format output"
  (let* ((clean-split (pretty-formatter-clean-input input))
	 (top-border (first clean-split))
	 (title-row (second clean-split))
	 (bottom-row (third clean-split))
	 (data-row (fourth clean-split))
	 (positions (pretty-formatter-positions data-row))
	 (title-row-split (pretty-formatter-title-row-split title-row))
	 (clean-split-string))
    (setf clean-split (cdddr clean-split))
    (setf clean-split-string (format nil "~{~a~%~}" clean-split))
    (setf clean
	  (concatenate
	   'string
	   (pretty-formatter-border positions
				    #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT
				    #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT
				    #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
				    #\BOX_DRAWINGS_LIGHT_HORIZONTAL)
	   (pretty-formatter-title-row positions title-row-split)
	   (pretty-formatter-border positions
				    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
				    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
				    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL
				    #\BOX_DRAWINGS_LIGHT_HORIZONTAL)
	   clean-split-string))
    (values clean)))

(defun pretty-formatter-clean-input (input)
  (let ((clean input))
    (setf clean (remove #\ESC input))
    (setf clean (regex-replace-all "\\[[0-1]{1}m" clean ""))
    (uiop:split-string clean :separator '(#\Newline))))

(defun pretty-formatter-positions (data-row)
  (loop for i across data-row
	for j from 0 upto (length data-row)
	when (string= "│" i)
	  collect j))

(defun pretty-formatter-title-row-split (title-row)
  (loop for i in (uiop:split-string title-row)
	when (and (not (string= i "")) (not (string= i "┃")))
	  collect i))

(defun pretty-formatter-title-row (positions title-row-split)
  (let ((new-title-row (make-array 0
				   :element-type 'character
				   :fill-pointer 0
				   :adjustable t)))
    (loop for i from 0 to (car (last positions))
	  when (or (= i 0) (= i (car (last positions))))
	    do (vector-push-extend #\│ new-title-row)
	  when (and (member i positions) (> i 0) (< i (car (last positions))))
	    do (vector-push-extend #\│ new-title-row)
	  when (not (member i positions))
	    do (vector-push-extend #\Space new-title-row))
    (loop for i in positions
	  for j from 0 upto (- (length title-row-split) 1)
	  do (replace new-title-row (nth j title-row-split) :start1 (+ i 2)))
    (vector-push-extend #\Newline new-title-row)
    (values new-title-row)))

(defun pretty-formatter-border (positions start end middle line)
  (let ((new-border (make-array 0
				       :element-type 'character
				       :fill-pointer 0
				       :adjustable t)))
    (loop for i from 0 to (car (last positions))
	  when (= i 0)
	    do (vector-push-extend start new-border)
	  when (= i (car (last positions)))
	    do (vector-push-extend end new-border)
	  when (and (member i positions) (> i 0) (< i (car (last positions))))
	    do (vector-push-extend middle new-border)
	  when (not (member i positions))
	    do (vector-push-extend line new-border))
    (vector-push-extend #\Newline new-border)
    (values new-border)))

(defun tab-separated-formatter (input)
  "Process TabSeparated format into a list of lists."
	(let ((tab-separated))
		(dolist (x (uiop:split-string input :separator '(#\Newline)))
			(push (uiop:split-string x :separator '(#\Tab)) tab-separated))
		(values tab-separated)))

(defun ver (val)
  "Boolean coercion helper."
  (not (not val)))
