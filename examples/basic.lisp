(defpackage #:until-it-dies.examples.basic
  (:use :cl))
(in-package #:until-it-dies.examples.basic)

(defclass my-engine (uid:engine)
  ()
  (:default-initargs :fps-limit 60))

(defclass my-window (uid:window)
  ()
  (:default-initargs
   :clear-color (uid:mix-colors uid:*blue* uid:*white* uid:*green*)
    :width 400
    :height 400))

(defmethod uid:on-draw ((window my-window))
  (uid:clear window)
  (uid:draw-rectangle (- (/ (uid:right-edge (uid:view window)) 2) 25)
                      (- (/ (uid:top-edge (uid:view window)) 2) 25)
                      50 50 :color uid:*red*))

(defmethod uid:on-key-down ((window my-window) keycode keysym string)
  (format t "~&Keycode: [~S], Keysym: [~S], String: [~S]~%" keycode keysym string)
  (when (eq keysym :escape)
    (uid:close-window window)))

(defparameter *engine* (make-instance 'my-engine :windows (list (make-instance 'my-window))))

(defun run ()
  (uid:run *engine*))
