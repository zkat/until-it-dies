(defpackage #:until-it-dies.examples.basic
  (:use :cl)
  (:export :run))
(in-package #:until-it-dies.examples.basic)

(defclass my-basic-game (uid:simple-game-engine)
  ()
  (:default-initargs :title "Basic UID Example"
    :fps-limit 60
    :clear-color (uid:mix-colors uid:*blue* uid:*white* uid:*green*)
    :width 400
    :height 400))

(defmethod uid:on-draw ((game my-basic-game))
  (uid:clear game)
  (uid:draw-rectangle (- (uid:width game) 25)
                      (- (uid:height game) 25)
                      50 50 :color uid:*red*))

(defmethod uid:on-key-down ((game my-basic-game) keycode keysym string)
  (format t "~&Keycode: [~S], Keysym: [~S], String: [~S]~%" keycode keysym string)
  (when (eq keysym :escape)
    (uid:close-window game)))

(defparameter *game* (make-instance 'my-basic-game))

(defun run ()
  (uid:run *game*))
