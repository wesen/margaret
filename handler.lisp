(in-package :margaret)

(defun revlist (&rest l)
  (nreverse l))

(define-method-combination revlist
  :operator revlist
  :identity-with-one-argument nil)

(defgeneric page-body (handler method request)
  (:method-combination revlist))
(defgeneric page-header (handler method request)
  (:method-combination revlist))
(defgeneric send-page (handler method request))

;;;; Standard handler

(defgeneric rss-feed (handler method request))

(defclass margaret-rss-handler (handler)
  ())

(defmethod handle-request-response ((handler margaret-rss-handler) method request)
  (request-send-headers request :content-type "xml/rss")
  (write-sequence "<?xml version=\"1.0\"?>" (request-stream request))
  (write-sequence (rss-feed handler method request) (request-stream request)))

(defclass margaret-handler (handler) 
  ((title :initarg :title :reader margaret-handler-title)))

(defmethod send-page ((handler margaret-handler) method request)
  (write-sequence 
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">"
   (request-stream request))
  (write-sequence
   (html
    `(html
      (head
       ((link :rel "stylesheet"
	      :href ,(urlstring (merge-url *margaret-url*
					   "files/margaret.css"))
	      :type "text/css"))
       ((meta :http-equiv "Content-Type"
	      :content "text/html; charset=iso-8859-1"))
       (title ,(margaret-handler-title handler)))
      (body
       (h1 ,(margaret-handler-title handler))
       ((div :class header)
	,@(page-header handler method request))
       (hr)
       ((div :class body)
	,@(page-body handler method request)))))
   (request-stream request)))

(defmethod page-header revlist ((handler margaret-handler) method request)
  `((div :class standard)
    ((a :href ,(urlstring *margaret-url*)) #l"Thread index")
    ((a :href ,(urlstring (merge-url *margaret-url* "blog")))
			  #l"Weblog")
    ((a :href ,(urlstring (merge-url *margaret-url* "user")))
			  #l"User homepage")))

(defmethod handle-request-response ((handler margaret-handler) method request)
  (request-send-headers request)
  (send-page handler method request))


;;;; Authentication handler

(defclass margaret-auth-handler (margaret-handler)
  ((capabilities :initarg :capabilities :initform nil
		 :reader margaret-handler-capabilities)))

(defgeneric authentication-check (handler method request))
(defgeneric page-unauth-body (handler method request))

(defmethod handle-request-authentication
  ((handler margaret-auth-handler) method request)
  (with-cookies-and-body-params (username password logout) request
     (if (and username password (not logout))
	 (setf (request-user request)
	       (get-user-checked (urlstring-escape username)
				 (urlstring-escape password)
				 (margaret-handler-capabilities handler)))
       (setf (request-user request) nil))))

(defmethod handle-request-response :around
  ((handler margaret-auth-handler) method request)
  (form-case request
     (logout (let ((cookie (list (make-clear-cookie 'username)
				 (make-clear-cookie 'password))))
	       (request-redirect request (urlstring *margaret-url*)
				 :set-cookie cookie)))
     (login (with-body-params (username password) request
	       (let ((cookie (list (make-cookie 'username username)
				   (make-cookie 'password password))))
		 (request-send-headers request :set-cookie cookie)
		 (send-page handler method request))))
     (t (call-next-method))))

(defmethod authentication-check
  ((handler margaret-auth-handler) method request)
  (request-user request))

(defmethod page-body :around ((handler margaret-auth-handler) method request)
  (if (authentication-check handler method request)
      (call-next-method)
    (page-unauth-body handler method request)))

(defmethod page-unauth-body ((handler margaret-auth-handler) method request)
  #l"Not authorized!")

(defmethod page-header revlist ((handler margaret-auth-handler) method request)
  `((span :class auth-header)
    ,@(let ((user (request-user request)))
	(if user
	    `(#l"Welcome, "
		((a :href ,(urlstring (merge-url *margaret-url* "user")))
		 ,(html-escape (user-name user))))
	  `(((form :method post)
	     ((table :class form-table)
	      (tr (td #l"Username:")
		  (td ((input :type text :size 15 :name username))))
	      (tr (td #l"Password:")
		  (td ((input :type password :size 15 :name password))))
	      (tr ((td :colspan 2)
		   ((input :type submit
			   :name login
			   :value #l"Login")))))))))))

;;;; Install handlers

(defun init-handlers ()
  (install-handler *root-handler*
		   (make-instance 'static-file-handler
				  :pathname *margaret-files-dir*)
		   (merge-url *margaret-url* "files/") nil))
