(defpackage #:uid-demo
  (:use :cl :sheeple :until-it-dies))
(in-package :uid-demo)

(defsheep =test-engine= (=engine=)
  ((title "Test Engine")
   (window-width 600)
   (window-height 600)))

(defparameter *test-image*
  (create-image "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png"))
(defparameter *test-anim*
  (create-animation "/home/zkat/hackery/lisp/until-it-dies/res/explosion.png"
                    15 14 0.05 14))

(defparameter *x* 50)
(defparameter *y* 50)
(defparameter *speed* 300)

(defmessage update ((engine =test-engine=) dt)
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

(defmessage draw ((engine =test-engine=))
  (declare (ignore engine))
  (with-color *green*
    (dotimes (i 1000)
      (draw-point (make-point :x (random 600)
                              :y (random 600)
                              :z 0))))
  (let ((scale-factor 5))
    (draw-sprite "Yo listen up here's the story, about a little guy..."
                 60 50 :x-scale scale-factor :y-scale scale-factor)
    (draw-sprite *test-anim* *x* *y* :x-scale scale-factor :y-scale scale-factor)))


