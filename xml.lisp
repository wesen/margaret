(in-package :margaret)

(defun xml-escape (xml-string)
  (apply #'concatenate 'string
         (loop for c across xml-string
	     collect (case c
		       ((#\<) "&lt;")
		       ((#\>) "&gt;")
		       ((#\&) "&amp;")
		       ((#\') "&apos;")
		       ((#\") "&quot;")
		       (t (string c))))))

(defun stringize (s &key (downcase nil))
  (cond ((symbolp s) (if downcase
		       (string-downcase (string s))
		       (string s)))
	((typep s 'string) s)
	(t (princ-to-string s))))

(defun sconc (downcase &rest args)
  "Concatenate ARGS as strings"
  (declare (optimize (speed 3)))
  (apply #'concatenate 'string (mapcar #'(lambda (s) (stringize s :downcase downcase))
				       args)))

(defun xml-attr (attr &key (downcase t))
  (labels ((flatten (a) (if (consp a)
			  (apply #'sconc downcase (mapcar #'flatten a))
			  a)))
    (let ((r (apply #'concatenate 'string
		    (loop for a on attr by #'cddr
			append (if (cadr a)
				 (list " " (stringize (car a) :downcase downcase)
				       "=\"" (sconc downcase (flatten (cadr a))) "\"")
				 (list " " (stringize (car a) :downcase downcase)))))))
      (the simple-string r))))

(defun xml (xml &key (downcase t) (newline t))
  (cond ((and (consp xml) (null (car xml)))
	 (apply #'concatenate 'string
		(mapcar #'(lambda (x) (xml x :downcase downcase
					   :newline newline)) (cdr xml))))
	((consp xml)
	 (let* ((tag (if (consp (car xml))
			 (caar xml)
			 (car xml)))
		(tagname (stringize tag :downcase downcase))
		(tag< (cond ((string-equal tagname "xml") "<?")
			    ((string-equal tagname "DOCTYPE") "<!")
			    (t "<")))
		(tag> (cond ((string-equal tagname "xml") "?>")
			    ((string-equal tagname "DOCTYPE") ">")
			    (t "/>")))
		(attrs (if (consp (car xml))
			   (cdar xml)
			   ()))
		(content (cdr xml)))
	   (if (null content)
	     (concatenate 'string
	       tag< tagname
	       (xml-attr attrs :downcase downcase) tag>
	       (when newline (string #\newline)))
	     (concatenate 'string
	       "<" tagname
	       (xml-attr attrs :downcase downcase) ">"
	       (apply #'concatenate 'string
		      (mapcar #'(lambda (x) (xml x :downcase downcase
						 :newline newline)) content))
	       "</" tagname ">"
	       (when newline (string #\newline))))))
	(t xml)))