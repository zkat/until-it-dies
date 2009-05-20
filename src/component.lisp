(in-package :until-it-dies)

(defsheep =component= ()
  ((parent nil)
   (initialized-p nil)
   (visible-p t)
   (subcomponents nil)))

(defmessage update ((component =component=) dt)
  (values))
(defmessage draw ((component =component=))
  (values))

(defmessage attach ((component =component=) (game =game=))
  (push component (components game)))

(defmessage detach ((component =component=) (game =game=))
  (setf (components game)
	(delete component (components game))))

(defmessage attach ((component =component=) (engine =engine=))
  (let ((game (car (games engine))))
    (attach component game)))

(defmessage detach ((component =component=) (engine =engine=))
  (let ((game (car (games engine))))
    (detach component game)))