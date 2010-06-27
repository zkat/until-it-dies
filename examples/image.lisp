(defpackage #:until-it-dies.examples.image
  (:use :cl)
  (:export :run))
(in-package #:until-it-dies.examples.image)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defclass image-example (uid:simple-game-engine)
  ((image :initarg :image :accessor image))
  (:default-initargs :title "UID Image-drawing example."
    :width 400
    :height 400
    :image (make-instance 'uid:image
                          :texture-filepath
                          (merge-pathnames "lisplogo_alien_256.png"
                                           *resource-directory*))))

(defmethod uid:on-draw ((game image-example))
  (uid:clear game)
  (uid:draw (image game) :x 100 :y 100))

(defmethod uid:on-key-down ((game image-example) keycode keysym string)
  (declare (ignore keycode string))
  (when (eq keysym :escape)
    (uid:close-window game)))

(defparameter *game* (make-instance 'image-example))

(defun run ()
  (uid:run *game*))
