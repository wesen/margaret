(in-package #:margaret)

(defmacro with-margaret-db ((&key error-handler) &rest body)
  `(handler-case
    (let ((*default-database* (connect *margaret-db-spec*
				       :database-type *margaret-db-type*
				       :if-exists :old
				       :pool t)))
      (unwind-protect 
	  ,@body
	(disconnect)))
    (clsql-connect-error (cond) ,(if error-handler
				     error-handler
				   '(signal cond)))))

(defun date-to-sql (second minute hour day month year)
  (format nil "~a-~a-~a ~a:~a:~a" year month day hour minute second))

(defun current-date-to-sql ()
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (date-to-sql second minute hour day month year)))

(defun sql-escape-p (char)
  "Determine whether `char' needs to be escaped in an SQL string."
  (or (char= char #\')
      (char= char #\\)))

(defun sql-escape (string)
  "Escape `string' so that it can be used as an SQL string."
  (let ((grow-chars (count-if #'sql-escape-p string)))
    (if (zerop grow-chars)
        string
        (let ((new-string (make-array (+ (length string) grow-chars)
                                      :element-type 'character
                                      :fill-pointer 0)))
          (loop :for char across string
                :do (when (sql-escape-p char)
                      (vector-push #\\ new-string))
                    (vector-push char new-string))
          new-string))))

(defun to-sql (value)
  (cond ((stringp value)
	 (format nil "'~a'" (sql-escape value)))
	((null value)
	 "NULL")
	((symbolp value)
	 (format nil "'~a'" value))
	((consp value)
	 (format nil "'(~{~A~^ ~})'" value))
	(t (princ-to-string value))))

(defun sql-insert (table values &key fields)
  (format nil "INSERT INTO ~a~@[ (~{~A~^, ~})~] VALUES (~{~A~^, ~})"
	  table
	  fields
	  (mapcar #'to-sql values)))

(defun sql-select (table &key fields where order limit)
  (format nil "SELECT ~:[*~;~:*~{~A~^, ~}~] FROM ~a~@[ WHERE ~A~]~@[ ORDER BY ~A~] ~@[ LIMIT ~A~]"
	  fields
	  table
	  where
	  order
	  limit))

(defun sql-update (table field-values &key where)
  (format nil "UPDATE ~A SET ~{~A~^, ~}~@[ WHERE ~A~]"
	  table
	  (mapcar #'(lambda (fv) (format nil "~A=~A"
					 (first fv)
					 (to-sql (second fv))))
		  field-values)
	  where))

(defun sql-where (pred name value)
  (format nil "~A~A~A" name pred (to-sql value)))

(defun sql-and (&rest clauses)
  (format nil "~{~A~^ AND ~}" clauses))

(defun sql-order (field &key (direction 'asc))
  (format nil "~A ~A" field direction))

(defun sql-limit (&key (offset 0) (length 0))
  (format nil "~A,~A" length offset))

(defun init-db ()
  (disconnect-pooled))