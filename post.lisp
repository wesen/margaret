(in-package #:margaret)

(defclass post ()
  ((id :initarg :id :reader post-id)
   (title :initarg :title :reader post-title)
   (body :initarg :body :reader post-body)
   (author :initarg :author :reader post-author)
   (date :initarg :date :reader post-date)
   (children :initform nil :accessor post-children)
   (thread-id :initarg :thread-id :accessor post-thread-id)))

(defclass thread ()
  ((id :initarg :id :reader thread-id)
   (title :initarg :title :reader thread-title)
   (author :initarg :author :reader thread-author)
   (last-author :initarg :last-author :reader thread-last-author)
   (creation-date :initarg :creation-date :reader thread-creation-date)
   (last-date :initarg :last-date :reader thread-last-date)))

(defun get-threads (&key id offset (length 10))
  (with-margaret-db ()
     (map-query
      'list
      #'(lambda (new-id title author last-author creation-date last-date)
	  (make-instance 'thread
			 :id new-id
			 :title title
			 :author author
			 :last-author last-author
			 :creation-date creation-date
			 :last-date last-date))
      (sql-select 'threads
		  :fields
		  '(id title author last_author creation_date last_date)
		  :where (when id
			   (sql-where '= 'id id))
		  :order (sql-order 'last_date :direction 'desc)
		  :limit (when offset
			   (sql-limit :offset offset :length length))))))

(defun get-thread (id)
  (first (get-threads :id id)))

(defun new-thread (title body
			 &key author
			 (date (current-date-to-sql)))
  (with-margaret-db ()
     (execute-command (sql-insert 'newthread
				  (list title body author date)
				  :fields '(title body author date)))))

(defun get-posts (&key id children recurse thread-id)
  (with-margaret-db ()
     (map-query
      'list
      #'(lambda (new-id title body author date thread-id)
	  (let ((post (make-instance 'post
				     :id new-id
				     :title title
				     :body body
				     :author author
				     :date date
				     :thread-id (parse-integer thread-id))))
	    (when recurse
	      (setf (post-children post) (get-posts :id new-id
						    :thread-id thread-id
						    :children t
						    :recurse recurse)))
	    post))
      (let (where-clauses)
	(when id
	  (push (if children
		    (sql-where '= 'parent id)
		  (sql-where '= 'id id)) where-clauses))
	(when thread-id
	  (push (sql-where '= 'thread thread-id) where-clauses))
	(sql-select 'posts
		    :fields '(id title body author date thread)
		    :where (when where-clauses
			       (apply #'sql-and where-clauses)))))))

(defun get-post (id &key recurse thread-id)
  (first (get-posts :id id :thread-id thread-id :recurse recurse)))

(defun get-root-post (thread-id &key recurse)
  (first (get-posts :id 0 :children t :thread-id thread-id :recurse recurse)))

(defun new-post (title body
		       &key author thread-id
		       (date (current-date-to-sql))
		       (parent 0))
  (with-margaret-db ()
     (execute-command (sql-insert 'newpost
				  (list title body date author parent
					thread-id)
				  :fields '(title body date author parent
						  thread)))))

(defun post-to-html (post)
  `((div :class post)
    ((table :class post-header)
     (tr ((td :class title) ,(html-escape (post-title post))
	  ((span :class commands)
	   ((a :href ,(urlstring (merge-url *margaret-url*
					    (format nil "newpost?id=~a"
						    (post-id post)))))
	    #l"Reply"))))
     (tr ((td :class author) #l"by "
	  ,(if (post-author post)
	       (html-escape (post-author post))
	     #l"anonymous")))
     (tr ((td :class date) #l"on "
	  ,(post-date post)))
      (tr ((td :class post-body) ,(html-escape (post-body post)))))
    ,(if (post-children post)
	 `(dl ,@(mapcar #'(lambda (child) `(dd ,(post-to-html child)))
			(post-children post)))
       `(br))))

(defun thread-to-html (thread)
  `((div :class thread)
    ((table :class thread-header)
     (tr ((td :class title) ((a :href ,(urlstring
					(merge-url
					 *margaret-url*
					 (format nil "thread?id=~a"
						 (thread-id thread)))))
			     ,(html-escape (thread-title thread)))))
     (tr ((td :class author) #l"created by "
	  ,(if (thread-author thread)
	       (html-escape (thread-author thread))
	     #l"anonymous")))
     (tr ((td :class date) #l"on "
	  ,(thread-creation-date thread)))
     (tr ((td :class author) #l"last modified by "
	  ,(if (thread-last-author thread)
	       (html-escape (thread-last-author thread))
	     #l"anonymous")))
     (tr ((td :class date) #l"on "
	  ,(thread-last-date thread))))))