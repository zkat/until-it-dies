(defpackage #:until-it-dies.examples.basic-no-subclass
  (:use :cl)
  (:export :run))
(in-package #:until-it-dies.examples.basic-no-subclass)

;; This example is mostly identical to the basic example, but it doesn't create new classes.
;;
;; It may seem simpler to do things this way, but actually writing your own subclasses is
;; probably more appropriate for actual applications (stuff beyond very basic example demos).
;;
;; Nevertheless, the instance-only approach is certainly possible, as shown here.
;;

(defparameter *window* (make-instance 'uid:window
                                      :title "Basic UID Example (no subclasses)"
                                      :clear-color (uid:mix-colors uid:*blue* uid:*white* uid:*green*)
                                      :width 400
                                      :height 400))

(defparameter *engine* (make-instance 'uid:engine
                                      :fps-limit 60
                                      :windows (list *window*)))

(defmethod uid:on-draw ((window (eql *window*)))
  (uid:clear window)
  (uid:draw-rectangle (- (/ (uid:right-edge (uid:view window)) 2) 25)
                      (- (/ (uid:top-edge (uid:view window)) 2) 25)
                      50 50 :color uid:*red*))

(defmethod uid:on-key-down ((window (eql *window*)) keycode keysym string)
  (format t "~&Keycode: [~S], Keysym: [~S], String: [~S]~%" keycode keysym string)
  (when (eq keysym :escape)
    (uid:close-window window)))

(defun run ()
  (uid:run *engine*))
