(in-package :until-it-dies)

(defsheep =component= ()
  ((parent nil)
   (subcomponents nil)
   (x 0)
   (y 0)
   (z 0)
   (width 0)
   (height 0)))

(defmessage update ((component =component=) dt)
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
   (z-velocity 0)))

(defmessage update ((mobile =mobile=) dt)
  (with-properties (x y z x-velocity y-velocity z-velocity) mobile
    (incf x (* x-velocity dt 1/1000))
    (incf y (* y-velocity dt 1/1000))
    (incf z (* z-velocity dt 1/1000))))

(defsheep =sprite= (=mobile=)
  ((texture nil)))

(defmessage draw ((sprite =sprite=))
  (with-properties (x y z width height texture) 
      sprite
    (bind-texture texture)
    (gl:with-primitives :quads
     (rectangle x y width height :z z))))
