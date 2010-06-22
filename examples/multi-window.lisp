(defpackage #:until-it-dies.examples.multi-window
  (:use :cl)
  (:export :run))
(in-package #:until-it-dies.examples.multi-window)

(defclass my-engine (uid:engine)
  ()
  (:default-initargs :fps-limit 60))

(defclass my-window (uid:window)
  ()
  (:default-initargs :clear-color (uid:mix-colors uid:*blue* uid:*white* uid:*green*)))

(defclass my-big-window (my-window)
  ()
  (:default-initargs
   :width 800 :height 600
   :title "Big UID Example Window"))

(defclass my-small-window (my-window)
  ()
  (:default-initargs
   :width 640 :height 480
   :title "Small UID Example Window"))

(defmethod uid:on-draw ((window my-window))
  (uid:clear window)
  (uid:draw-rectangle (- (/ (uid:right-edge (uid:view window)) 2) 25)
                      (- (/ (uid:top-edge (uid:view window)) 2) 25)
                      50 50 :color uid:*red*))

(defmethod uid:on-key-down :after ((window my-window) keycode keysym string)
  (declare (ignore keycode string))
  (when (eq keysym :escape)
    (uid:close-window window)))

(defmethod uid:on-key-down ((window my-big-window) keycode keysym string)
  (format t "~&Press on Big Window - Keycode: [~S], Keysym: [~S], String: [~S]~%"
          keycode keysym string))

(defmethod uid:on-key-down ((window my-small-window) keycode keysym string)
  (format t "~&Press on Small Window - Keycode: [~S], Keysym: [~S], String: [~S]~%"
          keycode keysym string))

(defparameter *engine* (make-instance 'my-engine
                                      :windows (list (make-instance 'my-big-window)
                                                     (make-instance 'my-small-window))))

(defun run ()
  (uid:run *engine*))
