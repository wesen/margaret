(in-package :margaret)

;;; new blog-post

(defclass margaret-blogrss-handler (margaret-rss-handler)
  ())

(defmethod rss-feed ((handler margaret-blogrss-handler) method request)
  (rss-to-xml (blog-to-rss)))

(defclass margaret-blogpost-handler (margaret-auth-handler)
  ())

(defmethod page-body revlist ((handler margaret-blogpost-handler)
			      method request)
  (labels ((post-form (&optional id (title "") (body ""))
	     `((div :class new-post)
		((form :method post :class post-form)
		 ,(if id 
		    `((input :type hidden
			     :name id
			     :value ,id))
		    "")
		 ((input :type text
			 :name title
			 :size 69
			 :value ,title))
		 (br)
		 ((textarea :rows 15 :cols 70 :name body) ,body)
		 (br)
		 ((input :type submit
			 :name newpost
			 :value "Post!"))))))
    (with-url-params ((id integer)) (request-url request)
      (if id
	(let ((post (get-blog-post id)))
	  (if post
	    `((div :class newblogpost)
	      ,(post-form (blog-post-id post)
			  (blog-post-title post)
			  (blog-post-body post)))
	    #l"No such posting"))
	`((div :class newblogpost)
	  ,(post-form))))))

(defmethod handle-request-response :around
  ((handler margaret-blogpost-handler) method request)
      (form-case request
	 (newpost (with-body-params ((id integer) title body) request
		    (let ((user (request-user request))
			  (post (when id (get-blog-post id))))
		      (when user
			(if post
			  (progn (setf (blog-post-title post) titl
				       (blog-post-body post) body
				       (blog-post-author post) (user-name user))
				 (update-blog-post post))
			  (new-blog-post title body
					 :author (user-name user)))
			(request-redirect
			 request
			 (urlstring (merge-url
				     *margaret-url* "blog"))))))))
      (call-next-method))

;;; blog handler

(defclass margaret-blog-handler (margaret-handler)
  ())

(defmethod page-header revlist ((handler margaret-blog-handler) method request)
  `(()
    ((a :href ,(urlstring (merge-url *margaret-url* "newblogpost")))
     #l"New blog post")
    ((a :href ,(urlstring (merge-url *margaret-url* "blog/rss.xml")))
     #l"Syndicate this site")))

  (defmethod page-body revlist ((handler margaret-blog-handler) method request)
  (with-url-params (offset) (request-url request)
     (if offset
	 (setf offset (parse-integer offset))
	 (setf offset 0))
     (let* ((posts (get-blog-posts :offset offset :length 5))
	    (cnt (length posts)))
       `((div :class blog)
	 ,(if (= cnt 0)
	      #l"No more posts"
	    `((dl :class blog-post-list)
	      ,@(mapcar #'(lambda (blog-post)
			    `(dd ,(blog-post-to-html blog-post)))
			posts)))
	 (br)
	 ,(if (>= offset 5)
	      `((a :href
		   ,(urlstring
		     (merge-url *margaret-url*
				(format nil "blog?offset=~A"
					(- offset 5)))))
		#l"Previous 5 posts")
	    "")
	 " "
	 ,(if (= cnt 5)
	      `((a :href ,(urlstring
			   (merge-url *margaret-url*
				      (format nil "blog?offset=~A"
					      (+ offset 5)))))
		#l"Next 5 posts")
	    "")
	 (ul (li ((a :href ,(urlstring (merge-url *margaret-url*
						  "newblogpost")))
		  #l"New blog post")))))))

(defun init-blog-handlers ()
  (install-handler *root-handler*
		   (make-instance 'margaret-blogpost-handler
		     :title #l"MARGARET New blog posting"
		     :capabilities '(blog))
		   (merge-url *margaret-url* "/newblogpost") nil)
  (install-handler *root-handler*
		   (make-instance 'margaret-blogrss-handler)
		   (merge-url *margaret-url* "/blog/rss.xml") nil)
  (install-handler *root-handler*
		   (make-instance 'margaret-blog-handler
				  :title #l"MARGARET Blog")
		   (merge-url *margaret-url* "/blog") nil))
  