(in-package #:cl-user)

(defpackage #:margaret-system (:use #:cl #:asdf))
(in-package #:margaret-system)

(defsystem margaret
  :depends-on (:clsql :clsql-postgresql :araneida :localization)
  :components ((:file "package")
  	       (:file "localization" :depends-on ("package"))
	       (:file "variables" :depends-on ("package" "localization"))
	       (:file "start-stop" :depends-on ("package" "localization"
	       	      		   	        "variables"))
	       (:file "html" :depends-on ("package" "localization" 
					  "variables"))
	       (:file "db" :depends-on ("package" "localization" "variables"))
	       (:file "user" :depends-on ("package" "localization"
					  "variables" "db"))
	       (:file "post" :depends-on ("package" "localization"
					  "variables" "db"))
	       (:file "handler" :depends-on
		      ("package" "variables" "localization" "html"))
	       (:file "user-handler" :depends-on
		      ("package" "variables" "localization" "html"
		       "handler" "user"))
	       (:file "xml" :depends-on
		      ("package"))
	       (:file "rss" :depends-on
		      ("package" "xml"))
	       (:file "blog" :depends-on ("package" "localization" "variables"
					  "db" "rss"))
	       (:file "blog-handler" :depends-on
		      ("package" "variables" "localization" "html"
		       "handler" "user" "blog"))
	       (:file "forum-handler" :depends-on
		      ("package" "variables" "localization" "html"
		       "handler" "user" "post"))))
	       
