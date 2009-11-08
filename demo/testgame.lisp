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
    (apply 'draw-at x y content args)))

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

(defproto *circle* =game-object=
  ((color (make-color :r 0.5 :g 0.2 :b 0.1))
   (x 100) (y 100) (dx/dt 0.0) (dy/dt 0.0)
   (radius 15)))

(defreply draw ((thing *circle*) &key)
  (with-properties (color x y radius) thing
    (with-color color
      (draw-circle (make-point x y) radius :filledp nil))))

(defreply update ((thing *circle*) dt &key)
  (declare (ignore dt))
  (with-properties ((x mouse-x) (y mouse-y)) =uid-demo=
    (with-properties (dx/dt dy/dt (cx x) (cy y)) *circle*
      (unless (< 0 cx (window-width =uid-demo=))
        (setf dx/dt (- dx/dt)))
      (incf cx dx/dt)
      (unless (< 0 cy (window-height =uid-demo=))
        (setf dy/dt (- dy/dt)))
      (incf cy dy/dt)
      (let* ((x-gap (- x cx))
             (y-gap (- y cy))
             (accel (/ (+ (expt x-gap 2) (expt y-gap 2)))))
        (incf dx/dt (* accel x-gap))
        (incf dy/dt (* accel y-gap))))))

(defreply update ((engine =uid-demo=) dt &key)
  (update *anim* dt)
  (update *circle* dt)
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
    (draw *anim* :x-scale scale-factor :y-scale scale-factor)
    (draw *circle*)
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
