(defpackage #:uid-text-rendering-example
  (:use :cl)
  (:nicknames #:uid-ex-text))
(in-package :uid-ex-text)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defclass text-engine (uid:engine)
  ())
(defclass text-window (uid:window)
  ())

(defparameter *our-font* (make-instance 'uid:font
                                        :filepath (merge-pathnames "example.ttf" *resource-directory*)
                                        :size 10))

(defmethod uid:on-draw ((window text-window))
  (uid:set-gl-window window)
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)

  (uid:draw-at 500 500 *alien*)

  (uid:with-font *our-font*
    (uid:draw-at 40 20 (make-instance 'uid:text :string "This is text")
                 :width 400 :height 400 :align :middle :valign :top)
    (uid:draw "Testing 1 2 3" :x 300 :y 400))
  (uid:swap-buffers window))

(defparameter *engine* (make-instance 'text-engine))

(defun run ()
  (setf (uid:windows *engine*) (list (make-instance 'text-window)))
  (uid:run *engine*))