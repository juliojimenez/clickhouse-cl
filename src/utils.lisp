(defpackage :clickhouse.utils
  (:use :cl :cl-ppcre)
  (:export :format-url
           :prettify
           :ver))

(in-package :clickhouse.utils)

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
    (cond (console (format t "~d" (pretty-formatter b)))
	  ((ver formatting) (cond ((equalp formatting clickhouse.ch-sql-parser::'json)
				   (json-formats b))
				  ((equalp formatting clickhouse.ch-sql-parser::'pretty)
				   (pretty-formatter b))
				  ((equalp formatting clickhouse.ch-sql-parser::'tabseparated)
				   (tab-separated-formatter b))))
	  (t (values b)))))

(defun pretty-formatter (input)
  "Clean up Pretty format output"
  (setq clean (remove #\ESC input))
  (setf clean (regex-replace-all "\\[[0-1]{1}m" clean ""))
  (setq clean-split (uiop:split-string clean :separator '(#\Newline)))
  (setq top-border (first clean-split))
  (setq title-row (second clean-split))
  (setq bottom-border (third clean-split))
  (setq data-row (fourth clean-split))
  (setq positions (loop for i across data-row
			for j from 0 upto (length data-row)
			when (string= "│" i)
			  collect j))
  (setq new-top-border (make-array 0
				   :element-type 'character
                                   :fill-pointer 0
                                   :adjustable t))
  (loop for i from 0 to (car (last positions))
	when (= i 0)
	  do (vector-push-extend #\┌ new-top-border)
	when (= i (car (last positions)))
	  do (vector-push-extend #\┐ new-top-border)
	when (and (member i positions) (> i 0) (< i (car (last positions))))
	  do (vector-push-extend #\┬ new-top-border)
	when (not (member i positions))
	  do (vector-push-extend #\─ new-top-border))
  (vector-push-extend #\Newline new-top-border)
  (setq new-title-row (make-array 0
				  :element-type 'character
				  :fill-pointer 0
				  :adjustable t))
  (setq title-row-split (uiop:split-string title-row))
  (setf title-row-split (loop for i in title-row-split
			      when (and (not (string= i "")) (not (string= i "┃")))
				collect i))
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
  
  (setf clean-split (cdddr clean-split))
  (setq clean-split-string (format nil "~{~a~%~}" clean-split))
  (setf clean (concatenate 'string new-top-border new-title-row bottom-border clean-split-string))
  (values clean))

(defun tab-separated-formatter (input)
  "Process TabSeparated format into a list of lists."
  (setq tab-separated nil)
  (dolist (x (uiop:split-string input :separator '(#\Newline)))
    (push (uiop:split-string x :separator '(#\Tab)) tab-separated))
  (values tab-separated))

(defun ver (val)
  "Boolean coercion helper."
  (not (not val)))
