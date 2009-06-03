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
   (height 0)
   (depth 0)
   (color nil))
  (:documentation
"A component is an object that can be drawn onto 
the screen. Components also accept the INIT, TEARDOWN, 
UPDATE, DRAW, ATTACH, and DETACH messages, which are
usually passed down by the screen and/or engine the component
is currently attached to."))

(defbuzzword scale (component factor)
  (:documentation "Scales the size of COMPONENT by FACTOR."))
(defmessage scale ((component =component=) factor)
  (with-properties (width height)
      component
    (setf width (* width factor))
    (setf height (* height factor))
    t))

(defmessage init ((component =component=))
  (declare (ignore component))
  (values))
(defmessage init :after ((component =component=))
  (mapc #'init (subcomponents component)))

(defmessage teardown ((component =component=))
  (declare (ignore component))
  (values))
(defmessage teardown :after ((component =component=))
  (mapc #'teardown (subcomponents component)))

(defmessage update ((component =component=) dt)
  (declare (ignore component dt))
  (values))

(defmessage draw :around ((component =component=))
  "We wrap the main DRAW message so that components are only drawn 
   if they are marked as visible."
  (when (visiblep component)
    (call-next-message)))

(defmessage draw ((component =component=))
  "We'll just draw a simple rectangle for default =component="
  (with-properties (x y z width height) 
      component
    (gl:with-primitives :quads
      (when (color component)
	(bind-color (color component)))
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
    (if screen
	(attach component screen)
	(error "Engine ~A does not have any screens attached." engine))))

(defmessage detach ((component =component=) (engine =engine=))
  "Ditto here -- they probably just want to attach COMPONENT to ENGINE's first screen."
  (let ((screen (first (screens engine))))
    (if screen
	(detach component screen)
	(error "Engine ~A does not have any screens attached." engine))))

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

(defmessage draw :before ((component =textured=))
  "Before we draw textured components, we should bind its texture."
  (when (texture component)
    (bind-texture (texture component))))

;;;
;;; Sprite prototype
;;;
(defsheep =sprite= (=textured= =mobile=)
  ()
  (:documentation
"Sprites are mobile, textured components that
are initialized to be the same size as the
texture they are drawn with."))

(defmessage init ((sprite =sprite=))
  (with-properties (height width texture)
    sprite
    (load-resource texture)
    (setf height (height texture))
    (setf width (width texture))))

