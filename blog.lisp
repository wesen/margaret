(in-package :margaret)

(defclass blog-post ()
  ((id :initarg :id :reader blog-post-id)
   (title :initarg :title :accessor blog-post-title)
   (body :initarg :body :accessor blog-post-body)
   (author :initarg :author :accessor blog-post-author)
   (date :initarg :date :accessor blog-post-date)))

(defun get-blog-posts (&key id (offset 0) (length 10))
  (with-margaret-db ()
     (map-query
      'list
      #'(lambda (new-id title author body date)
	  (make-instance 'blog-post
			 :id new-id
			 :title title
			 :author author
			 :date date
			 :body body))
      (sql-select 'blog :fields '(id title author body date)
		  :where (when id (sql-where '= 'id id))
		  :order (sql-order 'date :direction 'desc)
		  :limit (sql-limit :offset offset :length length)))))

(defun get-blog-post (id)
  (first (get-blog-posts :id id)))

(defun update-blog-post (post)
  (with-margaret-db ()
    (execute-command (sql-update 'blog
				 `((title ,(blog-post-title post))
				   (body ,(blog-post-body post))
				   (author ,(blog-post-author post))
				   (date ,(blog-post-date post)))
				 :where
				 (sql-where '= 'id (blog-post-id post))))))

(defun new-blog-post (title body
			    &key author
			    (date (current-date-to-sql)))
  (with-margaret-db ()
     (execute-command (sql-insert 'blog
				  (list title body author date)
				  :fields '(title body author date)))))

(defun blog-post-to-html (blog-post)
  `((div :class blog-post)
    ((a :name ,(blog-post-id blog-post)))
    (table (tr ((td :class title) 
		,(html-escape (blog-post-title blog-post))
		((span :class commands)
		 ((a :href ,(urlstring (merge-url *margaret-url*
						  (format nil "newblogpost?id=~a"
							  (blog-post-id blog-post)))))
		  #l"Modify"))))
	   (tr ((td :class author)
		#l"posted by " ,(html-escape (blog-post-author blog-post))))
	   (tr ((td :class date)
		#l"on " ,(blog-post-date blog-post)))
	   (tr ((td :class body)
		,(blog-post-body blog-post))))))

(defun blog-post-to-rss-item (blog-post)
  (let ((url (urlstring (merge-url *margaret-url*
				   (format nil "blog#~a" (blog-post-id blog-post))))))
  (make-instance 'rss-item
    :about url
    :title (blog-post-title blog-post)
    :link url
    :desc (blog-post-body blog-post))))

(defun blog-to-rss ()
  (let* ((blog-posts (get-blog-posts))
	 (rss-items (mapcar #'blog-post-to-rss-item blog-posts))
	 (rss-channel (make-instance 'rss-channel
			:about (urlstring (merge-url *margaret-url* "blog/rss.xml"))
			:title "MARGARET blog"
			:link (urlstring (merge-url *margaret-url* "blog"))
			:desc "The MARGARET blog"
			:items (mapcar #'rss-item-link rss-items))))
    (make-instance 'rss-feed :channel rss-channel
		   :items rss-items)))
			
    