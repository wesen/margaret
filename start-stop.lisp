(in-package #:margaret)

;;;; Start/Stop functions for MARGARET

(defun init-margaret ()
  (setf *content-types*
	(read-mime-types *mime-types-file*))
  (init-db)
  (init-users)
  (init-handlers)
  (init-user-handlers)
  (init-forum-handlers)
  (init-blog-handlers))

(defun start-margaret ()
  "Export the MARGARET server and start the request handlers."
  (init-margaret)
  (export-server *margaret-server*)
  (install-serve-event-handlers))

(defun stop-margaret ()
  "Stop the request handlers."
  (araneida::remove-handlers))