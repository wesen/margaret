(in-package :margaret)

;;;; Users handler

(defclass margaret-users-handler (margaret-auth-handler)
  ())

(defmethod page-body revlist ((handler margaret-users-handler) method request)
  `(ul ,@(mapcar #'(lambda (user) `(li ,(user-name user)))
		 *margaret-users*)))


;;;; User handler

(defclass margaret-user-handler (margaret-auth-handler)
  ())

(defmethod page-body revlist ((handler margaret-user-handler) method request)
  (let ((user (request-user request)))
    (form-case request
       (changepassword
	`((div :class user)
	  ,(with-body-params (password1 password2) request
	      (if (string-equal password1 password2)
		  (progn (setf (user-password user) password1)
			 #l"Password changed, please reload the page.")
		#l"Passwords were not equal - password not changed."))))
       (t
	`((div :class user)
	  (p #l"Welcome to your private page, "
	     ,(html-escape (user-name (request-user request)))
	     ".")
	  ((form :method post)
	   ((table :class form-table)
	    (th ((td :colspan 2) #l"Change your password:"))
	    (tr (td #l"Password:")
		(td ((input :type password :size 15 :name password1))))
	    (tr (td #l"Password (again):")
		(td ((input :type password :size 15 :name password2))))
	    (tr ((td :colspan 2)
		 ((input :type submit
			 :name changepassword :value #l"Change password"))))))
	  ((form :method post)
	   ((input :type submit :name logout :value #l"Logout")))
	  ,(if (member 'admin (user-capabilities user))
	       `(p ((a :href ,(urlstring (merge-url *margaret-url* "users")))
		    #l"User list"))))))))


(defun init-user-handlers ()
  (install-handler *root-handler*
		   (make-instance 'margaret-users-handler
				  :title #l"MARGARET Users"
				  :capabilities '(admin))
		   (merge-url *margaret-url* "/users") t)
  (install-handler *root-handler*
		   (make-instance 'margaret-user-handler
				  :title #l"MARGARET User page")
		   (merge-url *margaret-url* "/user") t))
