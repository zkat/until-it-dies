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
   (rotation 0)
   (color *white*))
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
  (with-properties (visiblep rotation) component
    (when visiblep
     (gl:with-pushed-matrix
       #+nil(when (/= rotation 0)
	 (gl:rotate rotation 0 0 1))
       (call-next-message)))))

(defmessage draw :before ((component =component=))
  "Since all components are colored, we bind its color before doing any actual drawing."
  (bind-color (color component)))

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
(defsheep =textured= ()
  ((texture =texture=))
  (:documentation
"Not to be confused with =texture=; =textured= is a mixin that provides
facilities for drawing textured onto components."))

(defbuzzword calculate-tex-coords (obj))
(defmessage calculate-tex-coords ((textured =textured=))
  (declare (ignore textured))
  (vector 0 0 1 1))

(defmessage draw :before ((textured =textured=))
  "Before we draw textured components, we should bind its texture."
  (when (texture textured)
    (bind-texture (texture textured))))

(defmessage draw ((textured =textured=))
  (with-properties (x y z width height color)
      textured
    (let ((tex-coords (calculate-tex-coords textured)))
      (when tex-coords
       (gl:with-primitives :quads
	 (when color
	   (bind-color color))
	 (rectangle x y width height :z z
		    :u1 (elt tex-coords 0)
		    :v1 (elt tex-coords 1)
		    :u2 (elt tex-coords 2)
		    :v2 (elt tex-coords 3)))))))

(defmessage draw :after ((textured =textured=))
  (unbind-texture (texture textured)))

;;;
;;; Image prototype
;;;
(defsheep =image= (=component= =textured=)
  ((texture (create-texture "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png")))
  (:documentation
"Images are textured components that
are initialized to be the same size as the
texture they are drawn with."))

(defun create-image (filepath &key (x 0) (y 0))
  (let* ((texture (create-texture filepath)))
    (clone (=image=)
	   ((x x)
	    (y y)
	    (texture texture)))))

(defmessage init ((image =image=))
  (with-properties (height width texture)
    image
    (load-resource texture)
    (setf height (height texture))
    (setf width (width texture))))

(defsheep =animation= (=image=)
  ((texture (create-texture "/home/zkat/hackery/lisp/until-it-dies/res/explosion.png"))
   (current-frame 0)
   (num-frames 14)
   (frame-delay 50)
   (timer 0)
   (frame-width 15)
   (frame-height 14)
   (frame-step 1)
   (animation-type :loop))
  (:documentation
"Animations are like images, but they use the provided texture
as a sprite sheet. Based on certain provided parameters, they
figure out which frames to draw."))

(defmessage init ((animation =animation=))
  "By default, an animation's actual height and width are based on the animation 
frame's height and width."
  (with-properties (height width frame-height frame-width texture)
      animation
    (setf height frame-height)
    (setf width frame-width)))

(defmessage update ((animation =animation=) dt)
  (with-properties (timer num-frames current-frame frame-delay animation-type frame-step)
      animation
    (incf timer dt)
    (when (> timer frame-delay)
      (setf timer 0)
      (case animation-type
	(:loop
	 (incf current-frame frame-step)
	 (when (or (> current-frame (1- num-frames))
		   (< current-frame 0))
	   (setf current-frame 0)))
	(:bounce
	 (incf current-frame frame-step)
	 (when (or (= current-frame num-frames)
		   (= current-frame 0))
	   (setf frame-step (* -1 frame-step)))
	 (when (or (> current-frame num-frames)
		   (< current-frame 0))
	   (setf current-frame 0)))
	(:once
	 (unless (= current-frame num-frames)
	   (incf current-frame frame-step))
	 (when (or (> current-frame num-frames)
		   (< current-frame 0))
	   (setf current-frame 0)))))))

(defmessage calculate-tex-coords ((animation =animation=))
  (with-properties (current-frame num-frames frame-width frame-height texture)
      animation
    (when (loadedp texture)
      (vector (/ (* (1- current-frame) frame-width) (width texture))
	      0 (/ (* current-frame frame-width) (width texture)) (/ frame-height (height texture))))))