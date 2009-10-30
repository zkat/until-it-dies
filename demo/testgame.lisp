(defpackage #:uid-demo
  (:use :cl :sheeple :until-it-dies))
(in-package :uid-demo)

(defproto =test-engine= (=engine=)
  ((title "Test Engine")
   (window-width 600)
   (window-height 600)))

(defparameter *test-image*
  (create-image "/home/adlai/src/until-it-dies/res/lisplogo_alien_256.png"))
(defparameter *test-anim*
  (create-animation "/home/adlai/src/until-it-dies/res/explosion.png"
                    15 14 0.05 14))

(defparameter *x* 50)
(defparameter *y* 50)
(defparameter *speed* 300)

(defreply update ((engine =test-engine=) dt &key)
  (update *test-anim* dt)
  (when (and (key-down-p :right)
             (< *x* (window-width engine)))
    (incf *x* (* *speed* dt)))
  (when (and (key-down-p :left)
             (< 0 *x*))
    (decf *x* (* *speed* dt)))
  (when (and (key-down-p :up)
             (< *y* (window-height engine)))
    (incf *y* (* *speed* dt)))
  (when (and (key-down-p :down)
             (< 0 *y*))
    (decf *y* (* *speed* dt))))

(defreply draw ((engine =test-engine=) &key)
  (declare (ignore engine))
  (let ((scale-factor 5))
    (with-color *green*
      (dotimes (i 1000)
        (draw-point (make-point :x (random 600)
                                :y (random 600)
                                :z 0))))
    (draw "HURR DURR HURR!"
          :x 60 :y 50 :x-scale scale-factor :y-scale scale-factor)
    (draw *test-anim* :x *x* :y *y* :x-scale scale-factor :y-scale scale-factor)))

(defreply mouse-down ((engine =test-engine=) button x y)
  (declare (ignore x y button engine))
  (values))