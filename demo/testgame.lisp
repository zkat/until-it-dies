(defpackage uid-demo
  (:use :cl :sheeple :until-it-dies :until-it-dies.demo.resource-info)
  (:shadow :speed)
  (:shadowing-import-from :uid :step)
  (:export :run-demo))
(in-package :uid-demo)

(defproto =uid-demo= (=engine=)
  ((title "UID Demo")
   (resizablep nil)
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

;; We use defproto here for convenience, but keep our *earmuffs*
(defproto *alien* =game-object=
  ((content (create-image (merge-pathnames "lisplogo_alien_256.png" *resource-directory*)))
   visiblep (x 255) (y 356)))

(defparameter *nay* (create-sound (merge-pathnames "sample.wav" *resource-directory*)))

(defreply draw :around ((thing *alien*) &key)
  (with-properties (visiblep) thing
    (when visiblep (call-next-reply))))

(defproto *anim* =game-object=
  ((content (create-animation (merge-pathnames "explosion.png" *resource-directory*) 15 14 0.05 14))
   (speed 300) (x 50) (y 50)))

(defproto *our-font* =font=
  ((filepath (merge-pathnames "example.otf" *resource-directory*))))

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
    (with-color (mix-colors *white* *black* *blue* *green*)
      (dotimes (i 1000)
        (draw-point (make-point (random 600) (random 600)))))
    (with-font *our-font*
      (draw "HURR DURR HURR!" :x 60 :y 50 :x-scale scale-factor :y-scale scale-factor :rotation 0))
    (draw-circle (make-point 100 100) 50)
    (draw *anim* :x-scale scale-factor :y-scale scale-factor)
    (draw *alien*)))

(defreply mouse-move :after ((engine =uid-demo=) x y)
  (with-properties ((alien-x x) (alien-y y)) *alien*
    (setf alien-x x alien-y y)))

(defreply mouse-down ((engine =uid-demo=) button)
  (with-properties ((alien-x x) (alien-y y) visiblep) *alien*
    (case button
      (0 (setf visiblep (not visiblep)))
      (1 (play *nay*)))))

(defun run-demo ()
  (run =uid-demo=))
