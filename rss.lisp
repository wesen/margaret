(in-package :margaret)

(defgeneric rss-to-xml (rss-element))

(defclass rss-feed ()
  ((channel :initarg :channel :accessor rss-feed-channel)
   (image :initform nil :initarg :image :accessor rss-feed-image)
   (items :initarg :items :accessor rss-feed-items)))

(defmethod rss-to-xml ((feed rss-feed))
  (html `()))

(defclass rss-channel ()
  ((about :initarg :about :accessor rss-channel-about)
   (title :initarg :title :accessor rss-channel-title)
   (link :initarg :link :accessor rss-channel-link)
   (desc :initarg :desc :accessor rss-channel-desc)
   (image :initform nil :initarg :image :accessor rss-channel-image)
   (textinput :initform nil :initarg :textinput :accessor rss-channel-textinput)
   (items :initform nil :initarg :items :accessor rss-channel-items)))

(defmethod rss-to-xml ((chan rss-channel))
  (html `((channel "rdf:about" ,(rss-channel-about chan))
	  (title ,(rss-channel-title chan))
	  (link ,(rss-channel-link chan))
	  (description ,(rss-channel-desc chan))
	  ,(if (rss-channel-image chan)
	     `((image "rdf:resource" ,(rss-channel-image chan)))
	     "")
	  ,(if (rss-channel-items chan)
	     `(items ("rdf:Seq" ,@(mapcar #'(lambda (item)
					      `(("rdf:li" "resource" ,item)))
					  (rss-channel-items chan))))
	     "")
	  ,(if (rss-channel-textinput chan)
	     `((textinput "rdf:resource" ,(rss-channel-textinput chan)))
	     ""))))

(defclass rss-image ()
  ((title :initarg :title :accessor rss-image-title)
   (url :initarg :url :accessor rss-image-url)
   (link :initarg :link :accessor rss-image-link)))

(defmethod rss-to-xml ((image rss-image))
  (html `()))

(defclass rss-item ()
  ((title :initarg :title :accessor rss-item-title)
   (link :initarg :link :accessor rss-item-link)
   (desc :initform nil :initarg :desc :accessor rss-item-desc)))

(defmethod rss-to-xml ((item rss-item))
  (html `()))

(defclass rss-textinput ()
  ((title :initarg :title :accessor rss-textinput-title)
   (desc :initarg :desc :accessor rss-textinput-desc)
   (link :initarg :link :accessor rss-textinput-link)
   (name :initarg :name :accessor rss-textinput-name)))

(defmethod rss-to-xml ((textinput rss-textinput))
  (html `()))

   
   