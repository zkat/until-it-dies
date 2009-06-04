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
;;; Colors for components
;;;
(defsheep =color= ()
  ((r 1)
   (g 1)
   (b 1)
   (a 1))
  (:documentation "A =color= is an object that represents a certain RGBA value.
The values are used directly by opengl, and should range between 0 and 1 (instead of 0-255)"))

(defun make-color (&key (r 1) (g 1) (b 1) (a 1))
  "A utility function for easily generating =color= objects."
  (clone (=color=) ((r r) (g g) (b b) (a a))))

;; Some standard colors
(defsheep =black= (=color=) 
  ((r 0) (g 0) (b 0)))
(defsheep =white= (=color=)
  ((r 1) (g 1) (b 1)))
(defsheep =magenta= (=color=)
  ((r 1) (g 0) (b 1)))
(defsheep =red= (=color=)
  ((r 1) (g 0) (b 0)))
(defsheep =green= (=color=)
  ((r 0) (g 1) (b 0)))
(defsheep =blue= (=color=)
  ((r 0) (g 0) (b 1)))

(defun bind-color (color)
  (with-properties (r g b a)
      color
    (gl:color r g b a)))


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
(defsheep =textured= ()
  ((texture =texture=))
  (:documentation
"Not to be confused with =texture=; =textured= is a mixin that provides
texture binding to drawable objects."))

(defmessage draw :before ((textured =textured=))
  "Before we draw textured components, we should bind its texture."
  (when (texture textured)
    (bind-texture (texture textured))))

;;;
;;; Sprite prototype
;;;
(defsheep =sprite= (=mobile= =textured=)
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

(defsheep =animated= (=textured=)
  ((texture (create-texture "/home/zkat/hackery/lisp/until-it-dies/res/explosion.png"))
   (current-frame 0)
   (num-frames 14)
   (frame-delay 50)
   (timer 0)
   (frame-width 15)
   (frame-height 14)
   (frame-step 1)
   (animation-type :loop)))

(defmessage update :before ((animated =animated=) dt)
  (with-properties (timer num-frames current-frame frame-delay animation-type frame-step)
      animated
    (incf timer dt)
    (when (> timer frame-delay)
      (incf current-frame frame-step)
      (setf timer 0)
      (when (> current-frame (1- num-frames))
	(case animation-type
	 (:loop
	  (setf current-frame 0))
	 (:bounce
	  (setf frame-step (* -1 frame-step)))
	 (:once
	  (setf frame-step 0)))))))

(defsheep =animated-sprite= (=mobile= =animated=)
  ((width 40)
   (height 40)
   (x 100)
   (y 100)))

(defmessage init ((sprite =animated-sprite=))
  (with-properties (height width texture)
    sprite
    (load-resource texture)
    (setf height (frame-height sprite))
    (setf width (frame-width sprite))))

(defmessage draw ((sprite =animated-sprite=))
  "We'll just draw a simple rectangle for default =component="
  (with-properties (x y z width height color)
      sprite
    (let ((tex-coords (calculate-tex-coords sprite)))
      (when tex-coords
       (gl:with-primitives :quads
	 (when color
	   (bind-color color))
	 (rectangle x y width height :z z
		    :u1 (elt tex-coords 0)
		    :v1 (elt tex-coords 1)
		    :u2 (elt tex-coords 2)
		    :v2 (elt tex-coords 3)))))))

(defun calculate-tex-coords (animated)
  (with-properties (current-frame num-frames frame-width frame-height texture)
    animated
    (when (loadedp texture)
      (vector (/ (* current-frame frame-width) (width texture))
	      0 (/ (* (1+ current-frame) frame-width) (width texture)) (/ frame-height (height texture))))))
