(in-package #:margaret)

(defvar *margaret-site-name* "androgyn.bl0rg.net"
  "Site name for MARGARET.")

(defvar *margaret-port* 4000
  "Internal port for the araneida server serving MARGARET.")

(defvar *margaret-url*
  (make-instance 'http-url
		 :host *margaret-site-name*)
  "URL for accessing MARGARET.")

(defvar *margaret-server*
  (make-instance 'server
		 :name *margaret-site-name*
		 :base-url *margaret-url*
		 :port *margaret-port*)
  "Server object for MARGARET.")

(defvar *mime-types-file*
  "/usr/local/etc/mime.types"
  "File containing mime type descriptions.")

(defvar *margaret-dir* #p"/home/manuel/lisp/margaret/"
  "Directory containing margaret installation.")

(defvar *margaret-files-dir*
  (merge-pathnames #p"files/" *margaret-dir*)
  "Directory containing static files.")

(defvar *margaret-db-spec*
  '("localhost" "test" "lisp" "lisp")
  "Connection string for MARGARET DB")

(defvar *margaret-db-type*
  :postgresql
  "Type of MARGARET DB")

;;;; Not to be modified

(defvar *margaret-users* nil)

(defvar *margaret-db* nil)