(in-package :until-it-dies)

(defsheep =component= ()
  ((parent nil)
   (visiblep t)
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
  "We'll just draw a simple rectangle for default =components="
  (when (visiblep component)
   (with-properties (x y z width height) 
       component
     (gl:with-primitives :quads
       (rectangle x y width height :z z)))))

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
   (x-accel 0)
   (y-accel 0)
   (z-accel 0)))

(defmessage update ((mobile =mobile=) dt)
  (with-properties (x y z x-velocity y-velocity z-velocity
		      x-accel y-accel z-accel) mobile
    ;; First, we adjust the velocity according to current acceleration
    (incf x-velocity (* x-accel dt 1/1000))
    (incf y-velocity (* y-accel dt 1/1000))
    (incf z-velocity (* z-accel dt 1/1000))
    ;; Then, we adjust the actual coordinates based on the velocity
    (incf x (* x-velocity dt 1/1000))
    (incf y (* y-velocity dt 1/1000))
    (incf z (* z-velocity dt 1/1000))))

(defsheep =textured= (=component=)
  ((texture nil)))

(defmessage draw :before ((component =textured=))
	    (when (texture component)
	      (bind-texture (texture component))))

(defsheep =sprite= (=mobile= =textured=)
  ())

(defmessage draw ((sprite =sprite=))
  (bind-texture (texture sprite))
  (when (visiblep sprite)
   (with-properties (x y z width height) 
       sprite
     (gl:with-primitives :quads
       (rectangle x y width height :z z)))))
