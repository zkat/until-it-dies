(defpackage #:until-it-dies.examples.basic-no-subclass
  (:use :cl)
  (:export :run))
(in-package #:until-it-dies.examples.basic-no-subclass)

;; This example is mostly identical to the basic example, but it doesn't create a new class.
;;
;; It may seem simpler to do things this way, but actually writing your own subclasses is
;; probably more appropriate for actual applications (stuff beyond very basic example demos).
;;
;; Nevertheless, the instance-only approach is certainly possible, as shown here.
;;

(defparameter *game* (make-instance 'uid:simple-game-engine
                                    :fps-limit 60
                                    :title "Basic UID Example (no subclasses)"
                                    :clear-color (uid:mix-colors uid:*blue* uid:*white* uid:*green*)
                                    :window-width 400
                                    :window-height 400))

(defmethod uid:on-draw ((game (eql *game*)))
  (uid:clear game)
  (uid:draw-rectangle (- (uid:width game) 25)
                      (- (uid:height game) 25)
                      50 50 :color uid:*red*))

(defmethod uid:on-key-down ((game (eql *game*)) keycode keysym string)
  (format t "~&Keycode: [~S], Keysym: [~S], String: [~S]~%" keycode keysym string)
  (when (eq keysym :escape)
    (uid:close-window game)))

(defun run ()
  (uid:run *game*))
