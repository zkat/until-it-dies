;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; sprite.lisp
;;;;
;;;; A sprite is a graphical object, be it an animation or an image.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Sprite prototype
;;;
(defproto =sprite= ())

(defun draw-at (x y obj &rest all-keys)
  (apply #'draw obj :x x :y y all-keys))

;;;
;;; Textured prototype
;;;
(defproto =textured= =sprite=
  ((texture =texture=))
  :documentation
  "Not to be confused with =texture=; =textured= is a mixin that provides
facilities for drawing textured onto components.")

(defreply calculate-tex-coords ((textured =textured=))
  (declare (ignore textured))
  (vector 0 0 1 1))

(defreply draw :before ((textured =textured=) &key x y )
  "Before we a draw textured sprite, we should bind its texture."
  (declare (ignore x y))
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (when (texture textured)
    (bind-texture (texture textured))))

(defreply draw :after ((textured =textured=) &key x y)
  "Once we're done drawing it, we should unbind the texture."
  (declare (ignore x y))
  (unbind-texture (texture textured))
  (gl:disable :texture-2d))

;;;
;;; Image prototype
;;;
(defproto =image= =textured=
  (texture)
  :documentation
  "Images are textured components that are initialized to be the same size as the
texture they are drawn with. Their TEXTURE property should contain a texture.")

(defreply create ((image =image=) &key filepath)
  (defobject =image= ((texture (create-texture filepath)))))

(defun create-image (filepath)
  (let* ((texture (create-texture filepath)))
    (defobject (=image=) ((texture texture)))))

(defreply height ((image =image=))
  (with-properties (texture) image
    (height texture)))
(defreply width ((image =image=))
  (with-properties (texture) image
    (width texture)))
(defreply filepath ((image =image=))
  (with-properties (texture) image
    (filepath texture)))

(defreply draw ((image =image=)
                &key x y x-scale y-scale
                rotation (z 0))
  (let ((tex-coords (calculate-tex-coords image))
        (height (height image))
        (width (width image)))
    (when tex-coords
      (gl:with-pushed-matrix
        (gl:translate x y z)
        (when rotation
          (gl:rotate (- rotation) 0 0 1))
        (draw-rectangle 0 0 (* width (or x-scale 1)) (* height (or y-scale 1)) :z z
                        :u1 (elt tex-coords 0)
                        :v1 (elt tex-coords 1)
                        :u2 (elt tex-coords 2)
                        :v2 (elt tex-coords 3))))))

(defproto =animation= (=image=)
  ((current-frame 0)
   (num-frames 14)
   (frame-delay 50)
   (frame-width 15)
   (frame-height 14)
   (frame-step 1)
   (timer 0)
   (animation-type :loop))
  :documentation
  "Animations are like images, but they use the provided texture
as a sprite sheet. Based on certain provided parameters, they
figure out which frames to draw.")

(defreply height ((animation =animation=))
  (frame-height animation))
(defreply width ((animation =animation=))
  (frame-width animation))

(defreply create ((animation =animation=) &key filepath frame-width
                  frame-height frame-delay num-frames (type :loop))
  (defobject (=animation=)
      ((texture (create-texture filepath))
       (frame-width frame-width)
       (frame-height frame-height)
       (frame-delay frame-delay)
       (num-frames num-frames)
       (animation-type type))))

(defun create-animation (filepath frame-width frame-height frame-delay
                         num-frames &optional (type :loop))
  (defobject (=animation=)
      ((texture (create-texture filepath))
       (frame-width frame-width)
       (frame-height frame-height)
       (frame-delay frame-delay)
       (num-frames num-frames)
       (animation-type type))))

(defreply update ((animation =animation=) dt &key)
  ;; TODO - this needs to update an animation properly regardless of framerate.
  ;;        That probably means that frames should sometimes be skipped.
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

(defreply calculate-tex-coords ((animation =animation=))
  (with-properties (current-frame num-frames frame-width frame-height texture)
      animation
    (with-properties ((tex-height height) (tex-width width)) texture
      (when (loadedp texture)
        (vector (/ (* (1- current-frame) frame-width) tex-width)
                0 (/ (* current-frame frame-width) tex-width) (/ frame-height tex-height))))))

;;;
;;; Text prototype
;;;
(defproto =text= =sprite=
  ((string-to-draw "Hello World")))

(defreply draw ((string =string=)
                &key x y x-scale y-scale
                rotation (font *font*)
                wrap (z 0))
  (when wrap
    (warn "UID doesn't support wrapping of text right now."))
  (unless (loadedp font)
    (load-resource font))
  (gl:with-pushed-matrix
    (gl:translate x y z)
    (when rotation
      (gl:rotate (- rotation) 0 0 1))
    (gl:scale (or x-scale 1) (or y-scale 1) 1)
    (uid-ftgl:render-font (font-pointer font) string :all)))

(defreply draw ((text =text=)
                &key x y x-scale y-scale
                rotation (font *font*)
                wrap (z 1))
  (draw (string-to-draw text) x y :z z
        :x-scale x-scale
        :y-scale y-scale
        :rotation rotation
        :font font
        :wrap wrap))

(defreply create ((text =text=) &key (string ""))
  (defobject =text= ((string-to-draw string))))

(defun create-text (string)
  (defobject (=text=) ((string-to-draw string))))

