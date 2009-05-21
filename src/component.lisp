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
  (let ((screen (or (default-screen engine)
		    (car (screens engine)))))
    (attach component screen)))

(defmessage detach ((component =component=) (engine =engine=))
  (let ((screen (or (default-screen engine)
		    (car (screens engine)))))
    (detach component screen)))

(defsheep =2d-component= (=component=)
  ((x 0)
   (y 0)
   (height 0)
   (width 0)
   (rotation 0)))


