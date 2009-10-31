(defpackage #:uid-demo
  (:use :cl :sheeple :until-it-dies))
(in-package :uid-demo)

(defproto =uid-demo= (=engine=)
  ((title "UID Demo")
   (window-width 600)
   (window-height 600)))

(defproto =game-object= ()
  ((x 0)
   (y 0)
   content))

(defreply draw ((thing =game-object=) &rest args &key)
  (with-properties (x y content) thing
    (apply 'draw content :x x :y y args)))

(defreply update ((thing =game-object=) dt &rest args &key)
  (apply 'update (content thing) dt args))

(defvar *alien*
  (defobject =game-object=
      ((content (create-image (truename "res/lisplogo_alien_256.png")))
       visiblep (x 255) (y 356))))

(defreply draw :around ((thing *alien*) &key)
  (with-properties (visiblep) thing
    (when visiblep (call-next-reply))))

(defvar *anim*
  (defobject =game-object=
      ((content (create-animation (truename "res/explosion.png")
                                  15 14 0.05 14))
       (speed 300) (x 50) (y 50))))

(defreply update ((engine =uid-demo=) dt &key)
  (update *anim* dt)
  (with-properties (x y speed) *anim*
    (when (and (key-down-p :right)
               (< x (window-width engine)))
      (incf x (* speed dt)))
    (when (and (key-down-p :left)
               (< 0 x))
      (decf x (* speed dt)))
    (when (and (key-down-p :up)
               (< y (window-height engine)))
      (incf y (* speed dt)))
    (when (and (key-down-p :down)
               (< 0 y))
      (decf y (* speed dt)))))

(defreply draw ((engine =uid-demo=) &key)
  (let ((scale-factor 4))
    (with-color *green*
      (dotimes (i 10000)
        (draw-point (make-point :x (random 600)
                                :y (random 600)
                                :z 0))))
    (draw "HURR DURR HURR!" :x 60 :y 50 :x-scale scale-factor :y-scale scale-factor)
    (draw *anim* :x-scale scale-factor :y-scale scale-factor)
    (draw *alien*)))

(defreply mouse-move :after ((engine =uid-demo=) x y)
  (with-properties ((alien-x x) (alien-y y)) *alien*
    (setf alien-x x alien-y y)))

(defreply mouse-down ((engine =uid-demo=) button)
  (with-properties ((alien-x x) (alien-y y) visiblep) *alien*
    (case button
      (0 (setf visiblep t
               alien-x (mouse-x engine)
               alien-y (mouse-y engine)))
      (1 (setf visiblep (not visiblep))))))
