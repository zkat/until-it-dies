(in-package :until-it-dies)

(defsheep =component= ()
  ((parent nil)
   (subcomponents nil)
   (x 0)
   (y 0)
   (z 0)
   (velocity-x 0)
   (velocity-y 0)
   (velocity-z 0)))

(defmessage update ((component =component=) dt)
  "By default, UPDATE simply changes the x, y, and z positions of COMPONENT based on its
velocities for each."
  #+nil(with-properties (x y z (dx velocity-x) (dy velocity-y) (dz velocity-z))
      component
    (incf x (* (/ dt 1000) dx))
    (incf y (* (/ dt 1000) dy))
    (incf z (* (/ dt 1000) dz)))
  (values))

(defmessage draw ((component =component=))
  "We'll just draw red squares for default =components="
  (with-accessors ((x x) (y y)) component
    (gl:color 1 1 1)
    (gl:with-primitives :quads
     (rectangle x y 100 100))))

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
