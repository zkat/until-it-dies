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

(defmessage attach ((component =component=) (screen =screen=))
  (push component (components screen)))

(defmessage detach ((component =component=) (screen =screen=))
  (setf (components screen)
	(delete component (components screen))))

(defmessage attach ((component =component=) (engine =engine=))
  (let ((screen (car (screens engine))))
    (attach component screen)))

(defmessage detach ((component =component=) (engine =engine=))
  (let ((screen (car (screens engine))))
    (detach component screen)))