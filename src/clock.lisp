(in-package :uid)

(defclass clock ()
  ((last-time :accessor last-time)
   (max-times-stored :accessor max-times-stored :initform 10)
   (times :accessor times :initform nil)
   (fps-limit :accessor fps-limit :initform nil)
   (cumulative-time :initform 0 :accessor cumulative-time)))

