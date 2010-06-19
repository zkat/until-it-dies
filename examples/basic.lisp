(defpackage #:until-it-dies.examples.basic
  (:use :cl)
  (:nicknames :uid.ex.basic))
(in-package :uid.ex.basic)

(defclass my-engine (uid:engine)
  ())
(defclass my-window (uid:window)
  ())

(defmethod uid:on-draw ((window my-window))
  (uid:set-gl-window window)
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (uid:draw-rectangle (/ (uid:right-edge (uid:view window)) 2)
                      (/ (uid:top-edge (uid:view window)) 2)
                      50 50 :color uid:*red*)
  (uid:swap-buffers window))

(defparameter *engine* (make-instance 'my-engine))

(defun run ()
  (let ((window1 (make-instance 'my-window))
        (window2 (make-instance 'my-window)))
    (setf (uid:windows *engine*) (list window1 window2))
    (uid:run *engine*)))