;; This file is part of Until It Dies

;; component.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Component prototype
;;;
(defsheep =component= ()
  ((parent nil)
   (visiblep t)
   (subcomponents nil)
   (x 0)
   (y 0)
   (z 0)
   (width 0)
   (height 0))
  (:documentation
"A component is an object that can be drawn onto 
the screen. Components also accept the INIT, TEARDOWN, 
UPDATE, DRAW, ATTACH, and DETACH messages, which are
usually passed down by the screen and/or engine the component
is currently attached to."))

(defmessage init ((component =component=))
  (declare (ignore component))
  (values))

(defmessage teardown ((component =component=))
  (declare (ignore component))
  (values))

(defmessage update ((component =component=) dt)
  (declare (ignore component dt))
  (values))

(defmessage draw :around ((component =component=))
  "We wrap the main DRAW message so that components are only drawn 
   if they are marked as visible."
  (when (visiblep component)
    (call-next-message)))

;; FIXME - this method is not being called. There's probably a bug in Sheeple.
(defmessage draw ((component =component=))
  "We'll just draw a simple rectangle for default =component="
  (with-properties (x y z width height) 
      component
    (gl:with-primitives :quads
      (rectangle x y width height :z z))))

(defmessage attach ((component =component=) (screen =screen=))
  (pushnew component (components screen))
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

;;;
;;; Mobile prototype
;;;
(defsheep =mobile= (=component=)
  ((x-velocity 0)
   (y-velocity 0)
   (z-velocity 0)
   (x-accel 0)
   (y-accel 0)
   (z-accel 0))
  (:documentation 
"A mobile component is a component with acceleration
and velocity. Based on those values, the x, y, and z
positions are updated by the UPDATE message."))

(defmessage update ((mobile =mobile=) dt)
  (with-properties (x y z x-accel y-accel z-accel
		    x-velocity y-velocity z-velocity) 
      mobile
    ;; First, we adjust the velocity according to current acceleration
    (incf x-velocity (* x-accel dt 1/1000))
    (incf y-velocity (* y-accel dt 1/1000))
    (incf z-velocity (* z-accel dt 1/1000))
    ;; Then, we adjust the actual coordinates based on the velocity
    (incf x (* x-velocity dt 1/1000))
    (incf y (* y-velocity dt 1/1000))
    (incf z (* z-velocity dt 1/1000))))

;;;
;;; Textured prototype
;;;
(defsheep =textured= (=component=)
  ((texture =texture=))
  (:documentation
"Not to be confused with =texture=; =textured= is a component 
that can have a texture slapped on it. A :before message on
=textured= takes care of binding the texture."))

;; FIXME: this is broken too. Fuck.
(defmessage draw :before ((component =textured=))
  "Before we draw textured components, we should bind its texture."
  (when (texture component)
    (bind-texture (texture component))))

;;;
;;; Sprite prototype
;;;
;;; - TODO: I can't get the width/height of the texture until it gets loaded. Find a way to do it.
(defsheep =sprite= (=textured= =mobile=)
  ()
  (:documentation
"Sprites are mobile, textured components that
are initialized to be the same size as the
texture they are drawn with."))

(defmessage draw ((sprite =sprite=))
  (call-next-message sprite)
  #+nil(when (visiblep sprite)
    (bind-texture (texture sprite))
    (with-properties (x y z width height) 
	sprite
      (gl:with-primitives :quads
	(rectangle x y width height :z z)))))

