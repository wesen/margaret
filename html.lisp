(in-package #:margaret)

;;;; Functions, Macros

(defun make-cookie (name value &key
			 (expires "Sun, 01-Jun-2036 00:00:01 GMT")
			 (path (url-path *margaret-url*))
			 (domain (url-host *margaret-url*))
			 max-age)
  (format nil "~A=~A; path=~a; expires=~a; domain=~a~@[; max-age: ~a~]"
	  name value path expires domain max-age))

(defun make-clear-cookie (name)
  (make-cookie name "" :max-age 0))

(defmacro form-case (request &rest cases)
  (let ((req-body (gensym)))
    `(let ((,req-body (request-body ,request)))
       (cond 
	,@(mapcar #'(lambda (c)
		     (if (eql (car c) t)
			 `(t ,@(cdr c))
		       `((body-param ,(symbol-name (car c)) ,req-body)
			 ,@(cdr c))))
		  cases)))))

(defun parse-params (params key)
  (mapcar #'(lambda (param)
	      (cond ((and (consp param)
			  (= (length param) 2)
			  (eq 'integer (second param)))
		     `(,(car param)
		       (ignore-errors (parse-integer ,(funcall key (car param))))))
		    (t `(,param ,(funcall key param)))))
	  params))

(defmacro with-body-params ((&rest params) request &rest body)
  (let ((req-body (gensym)))
    `(let ((,req-body (request-body ,request)))
       (let ,(parse-params params 
	      #'(lambda (name) 
		  `(body-param ,(symbol-name name) ,req-body)))
	 ,@body))))

(defmacro with-url-params ((&rest params) url &rest body)
  `(let ,(parse-params params #'(lambda (name) `(first (url-query-param ,url ,(symbol-name name)))))
     ,@body))

(defmacro with-cookies ((&rest params) request &rest body)
  `(let ,(parse-params params 
	  #'(lambda (name) `(request-cookie ,request ,(symbol-name name))))
     ,@body))

(defmacro with-cookies-and-body-params ((&rest params) request &rest body)
  (let ((req-body (gensym)))
    `(let ((,req-body (request-body ,request)))
       (let ,(parse-params params
	      #'(lambda (name) `(or (body-param ,(symbol-name name)
						,req-body)
				    (request-cookie ,request
						    ,(symbol-name name)))))
	 ,@body))))



