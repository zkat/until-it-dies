(in-package :until-it-dies)

(defsheep =component= ()
  ((parent nil)
   (subcomponents nil)
   (x 0)
   (y 0)
   (z 0)
   (x-velocity 0)
   (y-velocity 0)
   (z-velocity 0)))

(defmessage update ((component =component=) dt)
  "By default, UPDATE simply changes the x, y, and z positions of COMPONENT based on its
velocities for each."
  (with-properties (x y z (dx x-velocity) (dy y-velocity) (dz z-velocity))
      component
    (incf x (* dt dx))
    (incf y (* dt dy))
    (incf z (* dt dz)))
  (values))

(defmessage draw ((component =component=))
  "We'll just draw red squares for default =components="
  (with-properties (x y) component
    (gl:color 1 0 0)
    (rectangle x y 10 10)))

(defmessage attach ((component =component=) (screen =screen=))
  (push component (components screen)))

(defmessage detach ((component =component=) (screen =screen=))
  (setf (components screen)
	(delete component (components screen))))

(defmessage attach ((component =component=) (engine =engine=))
  "If someone wants to be vague about it, we just assume they want the first screen."
  (let ((screen (first (screens engine))))
    (attach component screen)))

(defmessage detach ((component =component=) (engine =engine=))
  "Ditto here -- they probably just want to attach COMPONENT to ENGINE's first screen."
  (let ((screen (first (screens engine))))
    (detach component screen)))
