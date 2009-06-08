;; This file is part of Until It Dies

;; sprite.lisp
;;
;; A sprite is a graphical object, be it an animation or an image.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Sprite prototype
;;;
(defsheep =sprite= ()
  ())

(defbuzzword draw-sprite (sprite x y &key))

;;;
;;; Textured prototype
;;;
(defsheep =textured= (=sprite=)
  ((texture =texture=))
  (:documentation
"Not to be confused with =texture=; =textured= is a mixin that provides
facilities for drawing textured onto components."))

(defbuzzword calculate-tex-coords (obj))
(defmessage calculate-tex-coords ((textured =textured=))
  (declare (ignore textured))
  (vector 0 0 1 1))

(defmessage draw-sprite :before ((textured =textured=) x y &key)
  "Before we a draw textured sprite, we should bind its texture."
  (declare (ignore x y))
  (when (texture textured)
    (bind-texture (texture textured))))

(defmessage draw-sprite :after ((textured =textured=) x y &key)
  "Once we're done drawing it, we should unbind the texture."
  (declare (ignore x y))
  (unbind-texture (texture textured)))

;;;
;;; Image prototype
;;;
(defsheep =image= (=textured=)
  ((texture (create-texture "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png")))
  (:documentation
"Images are textured components that
are initialized to be the same size as the
texture they are drawn with."))

(defun create-image (filepath)
  (let* ((texture (create-texture filepath)))
    (clone (=image=) ((texture texture)))))

(defmessage height ((image =image=))
  (with-properties (texture) image
    (height texture)))
(defmessage width ((image =image=))
  (with-properties (texture) image
    (width texture)))
(defmessage filepath ((image =image=))
  (with-properties (texture) image
    (filepath texture)))

(defmessage draw-sprite ((image =image=) x y &key x-scale y-scale rotation-theta)
  (let ((tex-coords (calculate-tex-coords image))
        (height (height (texture image)))
        (width (width (texture image))))
    (when tex-coords
      (gl:with-pushed-matrix
        (gl:scale (or x-scale 1) (or y-scale 1) 1)
        (when rotation-theta
          (gl:rotate rotation-theta 0 0 1))
        (gl:with-primitives :quads
          (rectangle x y width height
                     :u1 (elt tex-coords 0)
                     :v1 (elt tex-coords 1)
                     :u2 (elt tex-coords 2)
                     :v2 (elt tex-coords 3)))))))

(defsheep =animation= (=image=)
  ((texture (create-texture "/home/zkat/hackery/lisp/until-it-dies/res/explosion.png"))
   (current-frame 0)
   (num-frames 14)
   (frame-delay 50)
   (frame-width 15)
   (frame-height 14)
   (frame-step 1)
   (timer 0)
   (animation-type :loop))
  (:documentation
"Animations are like images, but they use the provided texture
as a sprite sheet. Based on certain provided parameters, they
figure out which frames to draw."))

(defun create-animation (filepath frame-width frame-height frame-delay 
                         num-frames &optional (type :loop))
  (clone (=animation=)
         ((texture (create-texture filepath))
          (frame-width frame-width)
          (frame-height frame-height)
          (frame-delay frame-delay)
          (num-frames num-frames)
          (animation-type type))))

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
    (with-properties ((tex-height height) (tex-width width)) texture
      (when (loadedp texture)
        (vector (/ (* (1- current-frame) frame-width) tex-width)
                0 (/ (* current-frame frame-width) tex-width) (/ frame-height tex-height))))))

;;;
;;; Text prototype
;;;
(defsheep =text= (=sprite=)
  ((string-to-draw "Hello World")))

(defmessage draw-sprite ((string =string=) x y 
                         &key x-scale y-scale 
                         rotation (font *font*)
                         wrap)
  (when wrap
    (warn "UID doesn't support wrapping of text right now."))
  (unless (loadedp font)
    (load-resource font))
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    (gl:scale (or x-scale 1) (or y-scale 1) 1)
    (when rotation
      (gl:rotate rotation 0 0 1))
    (ftgl:render-font (font-pointer font) string :all)))

(defmessage draw-sprite ((text =text=) x y
                         &key x-scale y-scale
                         rotation (font *font*)
                         wrap)
  (when wrap
    (warn "UID doesn't support wrapping of text right now."))
  (unless (loadedp font)
    (load-resource font))
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    (gl:scale (or x-scale 1) (or y-scale 1) 1)
    (when rotation
      (gl:rotate rotation 0 0 1))
    (ftgl:render-font (font-pointer font) (string-to-draw text) :all)))

(defun create-text (string)
  (clone (=text=) ((string-to-draw string))))
