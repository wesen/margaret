(in-package #:margaret)

(defclass user ()
  ((name :initarg :name
	 :reader user-name)
   (password :initarg :password
	     :initform nil
	     :accessor user-password)
   (email :initarg :email
	  :initform nil
	  :accessor user-email)
   (capabilities :initarg :capabilities
	   :initform nil
	   :accessor user-capabilities))
  (:documentation
   "User of the MARGARET system, CAPABILITIES is a list of symbols,
for example: ADMIN"))


;;; MOP ??
(defmethod (setf user-password) :after (new-password (u user))
  (update-user u))

(defmethod (setf user-email) :after (new-email (u user))
  (update-user u))

(defmethod (setf user-capabilities) :after (new-capabilities (u user))
  (update-user u))

;;;; Functions

(defun load-users-from-db ()
  "Returns a list of the users stored in the DB."
  (with-margaret-db ()
     (map-query 'list #'(lambda (name password email capabilities)
			  (make-instance
			   'user
			   :name name
			   :password password
			   :email email
			   :capabilities
			   (when capabilities
			     (let ((*package* #.*package*))
			       (read-from-string capabilities)))))
		(sql-select 'users
			    :fields '(name password email capabilities)))))

(defun get-user (name)
  (find name *margaret-users* :key #'user-name :test #'string-equal))

(defun create-user (name &key password email capabilities)
  "Create a new user, pushes it into the user list and stores it in DB."
  (unless (get-user name)
    (let ((user (make-instance 'user
			       :name name
			       :email email
			       :password password
			       :capabilities capabilities)))
      (with-margaret-db ()
	 (push user *margaret-users*)
	 (execute-command
	  (sql-insert 'users
		      (list name password email capabilities)
		      :fields '(name password email capabilities))))
      user)))

(defun update-user (user)
  (with-margaret-db ()
     (execute-command (sql-update 'users
				  `((password ,(user-password user))
				    (email ,(user-email user))
				    (capabilities ,(user-capabilities user)))
				  :where
				  (sql-where "=" 'name (user-name user))))))

(defun password-check (user password)
  (string-equal password (user-password user)))

(defun get-user-checked (name password &optional capabilities)
  (let ((user (get-user name)))
    (when (and user (and (password-check user password)
			 (if capabilities
			     (subsetp capabilities (user-capabilities user))
			   t)))
      user)))
      
(defun init-users ()
  (setf *margaret-users* (load-users-from-db)))