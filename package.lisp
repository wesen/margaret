(in-package #:cl-user)

(defpackage #:margaret
  (:use #:cl #:araneida #:clsql-sys :localization)
  (:export #:start-margaret
	   #:stop-margaret))