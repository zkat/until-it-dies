(defpackage #:until-it-dies.examples.text
  (:use :cl)
  (:nicknames :uid.ex.text))
(in-package :uid.ex.text)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defclass text-engine (uid:engine)
  ())
(defclass text-window (uid:window)
  ())

(defparameter *our-font* (make-instance 'uid:font
                                        :filepath (merge-pathnames "example.ttf" *resource-directory*)
                                        :size 20))

(defmethod uid:on-draw ((window text-window))
  (uid:set-gl-window window)
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (uid:with-color uid:*black*
   (uid:with-font *our-font*

     (uid:draw-at 300 400 (make-instance 'uid:text :string "This is text")
                  :align :middle :valign :bottom)

     (uid:draw "Testing: 1, 2, 3" :x 300 :y 400 :x-scale 2 :y-scale 2)))

  (uid:swap-buffers window))

(defparameter *engine* (make-instance 'text-engine))

(defun run ()
  (setf (uid:windows *engine*) (list (make-instance 'text-window
                                                    :width 200
                                                    :height 200)))
  (uid:run *engine*))