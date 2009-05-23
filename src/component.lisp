(in-package :until-it-dies)

(defsheep =component= ()
  ((parent nil)
   (subcomponents nil)
   (x 0)
   (y 0)
   (z 0)))

(defmessage update ((component =component=) dt)
  "By default, UPDATE simply changes the x, y, and z positions of COMPONENT based on its
velocities for each."
  (declare (ignore component dt))
  (values))

(defmessage draw ((component =component=))
  "We'll just draw red squares for default =components="
  (declare (ignore component))
  (values))

(defmessage attach ((component =component=) (screen =screen=))
  (push component (components screen))
  (setf (parent component) screen))

(defmessage detach ((component =component=) (screen =screen=))
  (setf (components screen)
	(delete component (components screen)))
  (setf (parent component) nil))

(defmessage attach ((component =component=) (engine =engine=))
  "If someone wants to be vague about it, we just assume they want the first screen."
  (let ((screen (first (screens engine))))
    (attach component screen)))

(defmessage detach ((component =component=) (engine =engine=))
  "Ditto here -- they probably just want to attach COMPONENT to ENGINE's first screen."
  (let ((screen (first (screens engine))))
    (detach component screen)))

(defsheep =mobile= (=component=)
  ((x-velocity 0)
   (y-velocity 0)
   (z-velocity 0)
   (x-rotation 0)
   (y-rotation 0)
   (z-rotation 0)))
