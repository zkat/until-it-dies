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
(defclass sprite () ())

(defgeneric draw (obj &key))

(defun draw-at (x y obj &rest all-keys)
  (apply #'draw obj :x x :y y all-keys))

;;;
;;; Textured prototype
;;;
(defclass textured (sprite)
  ((texture :initarg :texture :accessor texture
            :initform (error "Textured objects must have a texture object associated with them.")))
  (:documentation "Not to be confused with TEXTURE; TEXTURED is a mixin that provides
facilities for drawing textures onto components."))

(defmethod height ((textured textured))
  (height (texture textured)))
(defmethod width ((textured textured))
  (width (texture textured)))
(defmethod filepath ((textured textured))
  (filepath (texture textured)))

(defgeneric calculate-tex-coords (texture)
  (:method ((textured textured))
    (vector 0 0 1 1)))

(defmethod draw :before ((textured textured) &key)
  "Before we a draw textured sprite, we should bind its texture."
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (when (texture textured)
    (bind-texture (texture textured))))

(defmethod draw :after ((textured textured) &key)
  "Once we're done drawing it, we should unbind the texture."
  (unbind-texture (texture textured))
  (gl:disable :texture-2d))

;;;
;;; Images
;;;
(defclass image (textured)
  ()
  (:documentation
   "Images are textured components that are initialized to be the same size as the
texture they are drawn with. Their TEXTURE property should contain a texture."))

(defmethod draw ((image image)
                 &key x y x-scale y-scale
                 rotation (z 0) (x-offset 0) (y-offset 0))
  (let ((x (+ x x-offset))
        (y (+ y y-offset))
        (tex-coords (calculate-tex-coords image))
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

(defclass animation (image)
  ((current-frame :initform 0 :initarg :current-frame :accessor current-frame)
   (num-frames :initarg :num-frames :accessor num-frames)
   (frame-delay :initarg :frame-delay :accessor frame-delay)
   (frame-width :initarg :frame-width :accessor frame-width)
   (frame-height :initarg :frame-height :accessor frame-height)
   (frame-step :initform 1 :initarg :frame-step :accessor frame-step)
   (timer :initform 0 :accessor timer)
   (animation-type :initform :loop :initarg :type :accessor animation-type))
  (:documentation
  "Animations are like images, but they use the provided texture
as a sprite sheet. Based on certain provided parameters, they
figure out which frames to draw."))

(defmethod height ((animation animation))
  (frame-height animation))
(defmethod width ((animation animation))
  (frame-width animation))

(defmethod on-update ((animation animation) dt)
  ;; TODO - this needs to update an animation properly regardless of framerate.
  ;;        That probably means that frames should sometimes be skipped.
  (with-accessors ((timer timer) (num-frames num-frames)
                   (current-frame current-frame) (frame-delay frame-delay)
                   (animation-type animation-type) (frame-step frame-step))
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

(defmethod calculate-tex-coords ((animation animation))
  (with-accessors ((current-frame current-frame) (num-frames num-frames)
                   (frame-width frame-width) (frame-height frame-height)
                   (texture texture))
      animation
    (with-accessors ((tex-height height) (tex-width width))
        texture
      (when (loadedp texture)
        (vector (/ (* (1- current-frame) frame-width) tex-width)
                0 (/ (* current-frame frame-width) tex-width) (/ frame-height tex-height))))))

;;;
;;; Text prototype
;;;
(defclass text (sprite)
  ((string-to-draw :initform "Hello World" :initarg :string :accessor string-to-draw)))

(defmethod draw ((string string)
                 &key x y x-scale y-scale
                 rotation (font *font*)
                 (z 0) (x-offset 0) (y-offset 0))
  (unless (loadedp font)
    (load-resource font))
  (gl:with-pushed-matrix
    (let ((x (+ x x-offset))
          (y (+ y y-offset)))
      (gl:translate x y z)
      (when rotation
        (gl:rotate (- rotation) 0 0 1))
      (gl:scale (or x-scale 1) (or y-scale 1) 1)
      (draw-string (font-pointer font) string :size (size font)))))

(defmethod draw ((text text)
                 &key x y width height
                 x-scale y-scale
                 rotation (wrap t) (align :left) (valign :bottom)
                 (font *font*) (z 0)
                 (x-offset 0) (y-offset 0))
  (unless (loadedp font)
    (load-resource font))
  (gl:with-pushed-matrix
    (let ((x (+ x x-offset))
          (y (+ y y-offset)))
      (gl:translate x y z)
      (when rotation
        (gl:rotate (- rotation) 0 0 1))
      (gl:scale (or x-scale 1) (or y-scale 1) 1)
      (mapc #'(lambda (line)
                (draw-at (units->pixels (first line) (font-pointer font) (size font))
                         (units->pixels (second line) (font-pointer font) (size font))
                         (third line) :font font))
            (format-text (string-to-draw text)
                         :width width
                         :height height
                         :font font
                         :wrap wrap
                         :align align
                         :valign valign)))))
