(in-package :margaret)

;;;; Frontpage handler

(defclass margaret-frontpage-handler (margaret-handler)
  ())

(defmethod page-header revlist ((handler margaret-frontpage-handler)
			     method request)
  `((a :href ,(urlstring (merge-url *margaret-url* "newthread")))
    #l"New thread"))

(defmethod page-body revlist ((handler margaret-frontpage-handler) method request)
  (with-url-params (offset) (request-url request)
     (if offset
	 (setf offset (parse-integer offset))
	 (setf offset 0))
     (let* ((threads (get-threads :offset offset :length 5))
	    (cnt (length threads)))
       `((div :class frontpage)
	 ,(if (= cnt 0)
	      #l"No more threads"
	    `((dl :class thread-list)
	      ,@(mapcar #'(lambda (thread) `(dd ,(thread-to-html thread)))
			threads)))
	 (br)
	 ,(if (>= offset 5)
	      `((a :href
		   ,(urlstring
		     (merge-url *margaret-url*
				(format nil "?offset=~A"
					(- offset 5)))))
		#l"Previous 5 threads")
	    "")
	 " "
	 ,(if (= cnt 5)
	      `((a :href ,(urlstring
			   (merge-url *margaret-url*
				      (format nil "?offset=~A"
					      (+ offset 5)))))
		#l"Next 5 threads")
	    "")))))

;;;; Thread handler

(defclass margaret-thread-handler (margaret-handler)
  ())

(defmethod page-body revlist ((handler margaret-thread-handler) method request)
  (with-url-params (id) (request-url request)
      (let ((thread (get-thread id)))
	(if thread
	    (let ((post (get-root-post id :recurse t)))
	      (if post
		  `((div :class thread)
		    ,(thread-to-html thread)
		    ,(post-to-html post))
		`((div :class thread)
		  #l"No such posting")))
	  `((div :class thread)
	    #l"No such thread")))))

;;;; Post handler

(defclass margaret-post-handler (margaret-handler)
  ())

(defmethod page-body revlist ((handler margaret-post-handler) method request)
  (with-url-params (id) (request-url request)
     (let ((post (get-post id :recurse t)))
       (if post
	   `((div :class post)
	     ,(post-to-html post))
	 `((div :class post)
	   #l"No such posting")))))

;;; New post handler

(defclass margaret-newthread-handler (margaret-auth-handler)
  ())

(defmethod page-body revlist ((handler margaret-newthread-handler) method request)
  `((div :class newthread)
    ((div :class new-post)
     ((form :method post :class post-form)
      ((input :type text
	      :name title
	      :size 69))
      (br)
      ((textarea :rows 15 :cols 70 :name body))
      (br)
      ((input :type submit
	      :name newthread
	      :value #l"Create new thread"))))))

(defmethod handle-request-response :around
  ((handler margaret-newthread-handler) method request)
  (form-case request
	     (newthread (with-body-params (title body) request
		   (let ((user (request-user request)))
		     (if user
			 (progn (new-thread title body
					    :author (user-name user))
				(request-redirect request
						  (urlstring *margaret-url*)))
		       (request-redirect
			request
			(urlstring (merge-url *margaret-url* "user")))))))
     (t (call-next-method))))

;;; New post handler

(defclass margaret-newpost-handler (margaret-auth-handler)
  ())

(defmethod page-body revlist ((handler margaret-newpost-handler) method request)
  (with-url-params (id) (request-url request)
     (if id
	 (let ((nid (parse-integer id)))
	   (labels ((post-form (title thread-id)
			       `((div :class new-post)
				 ((form :method post :class post-form)
				  ((input :type hidden
					  :name id
					  :value ,nid))
				  ((input :type hidden
					  :name thread-id
					  :value ,thread-id))
				  ((input :type text
					  :name title
					  :size 69
					  :value ,title))
				  (br)
				  ((textarea :rows 15 :cols 70 :name body))
				  (br)
				  ((input :type submit
					  :name newpost
					  :value "Post!"))))))
	     (let ((post (get-post nid :recurse nil)))
	       (if post
		   `((div :class newpost)
		     ,(post-to-html post)
		     ,(post-form (post-title post) (post-thread-id post)))
		 #l"No such posting")))))))

(defmethod handle-request-response :around
  ((handler margaret-newpost-handler) method request)
      (form-case request
	 (newpost (with-body-params (id thread-id title body) request
		 (let ((user (request-user request)))
		   (if user
		       (progn (new-post title body
					:author (user-name user)
					:parent id
					:thread-id thread-id)
			      (request-redirect
			       request
			       (urlstring (merge-url
					   *margaret-url*
					   (format nil "thread?id=~a"
						   thread-id)))))
		     (call-next-method)))))
	 (t (call-next-method))))

(defun init-forum-handlers ()
  (install-handler *root-handler*
		   (make-instance 'margaret-frontpage-handler
				  :title #l"MARGARET")
		   *margaret-url* nil)
  (install-handler *root-handler*
		   (make-instance 'margaret-post-handler
				  :title #l"MARGARET Postings")
		   (merge-url *margaret-url* "/post") nil)
  (install-handler *root-handler*
		   (make-instance 'margaret-thread-handler
				  :title #l"MARGARET Threads")
		   (merge-url *margaret-url* "/thread") nil)
  (install-handler *root-handler*
		   (make-instance 'margaret-newpost-handler
				  :title #l"MARGARET Reply to post")
		   (merge-url *margaret-url* "/newpost") nil)  
  (install-handler *root-handler*
		   (make-instance 'margaret-newthread-handler
				  :title #l"MARGARET New thread")
		   (merge-url *margaret-url* "/newthread") nil))
  