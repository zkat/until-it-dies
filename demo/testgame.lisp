(defpackage uid-demo
  (:use :cl :sheeple :until-it-dies :until-it-dies.demo.resource-info)
  (:shadow :speed)
  (:shadowing-import-from :uid :step)
  (:export :run-demo))
(in-package :uid-demo)

(defparameter *uid-demo* (create-engine :title "UID Demo"
                                        :resizablep nil
                                        :window-width 600
                                        :window-height 600))

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
  ((x 100) (y 100) (dx/dt 0.0) (dy/dt 0.0)
   (radius 15)))

(defreply draw ((thing *circle*) &key)
  (with-properties (x y radius dx/dt dy/dt) thing
    (flet ((ndist (a b) (abs (- a b))))
      (let* ((speed (sqrt (+ (* dx/dt dx/dt) (* dy/dt dy/dt))))
             (brightness (max (/ speed 4) 0.3))
             (theta (atan dy/dt dx/dt))
             ;; The following depends heavily upon the gory details of CL:ATAN
             ;; The net effect is that 0° = red, 120° = green, 240° = blue,
             ;; and headings in between are blended smoothly.
             (r (* (- 1 (/ (ndist theta 0) pi 2/3)) brightness))
             (g (* (- 1 (/ (ndist (mod theta (* 2 pi)) (* pi 2/3)) pi 2/3)) brightness))
             (b (* (- 1 (/ (ndist (mod theta (* 2 pi)) (* pi 4/3)) pi 2/3)) brightness)))
        (with-color (make-color :r r :b b :g g)
          (draw-circle (make-point x y) radius :filledp t))))))

(defreply update ((thing *circle*) dt &key)
  (declare (ignore dt))
  (with-properties ((x mouse-x) (y mouse-y)) *uid-demo*
    (with-properties (dx/dt dy/dt (cx x) (cy y)) *circle*
      (when (> (abs dx/dt) 5)
        (setf dx/dt (* 0.9 dx/dt)))
      (when (> (abs dy/dt) 5)
        (setf dy/dt (* 0.9 dx/dt)))
      (unless (< 0 cx (window-width *uid-demo*))
        (setf dx/dt (- dx/dt)))
      (incf cx dx/dt)
      (unless (< 0 cy (window-height *uid-demo*))
        (setf dy/dt (- dy/dt)))
      (incf cy dy/dt)
      (let* ((x-gap (- x cx))
             (y-gap (- y cy))
             (accel (/ (+ (expt x-gap 2) (expt y-gap 2)))))
        (incf dx/dt (* accel x-gap))
        (incf dy/dt (* accel y-gap))))))

(defreply update ((engine *uid-demo*) dt &key)
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

(defreply draw ((engine *uid-demo*) &key)
  (let ((scale-factor 4))
    (with-color (mix-colors *white* *black* *blue* *green*)
      (dotimes (i 1000)
        (draw-point (make-point (random 600) (random 600)))))
    (with-font *our-font*
      (draw-at 60 20 "Try left-clicking, right-clicking, and pressing the arrow keys!"
               :x-scale 1.5 :y-scale 1.5))
    (draw *anim* :x-scale scale-factor :y-scale scale-factor)
    (draw *circle*)
    (draw *alien*)))

(defreply mouse-move :after ((engine *uid-demo*) x y)
  (with-properties ((alien-x x) (alien-y y)) *alien*
    (setf alien-x x alien-y y)))

(defreply mouse-down ((engine *uid-demo*) button)
  (with-properties ((alien-x x) (alien-y y) visiblep) *alien*
    (case button
      (0 (setf visiblep (not visiblep)))
      (1 (play *nay*)))))

(defun run-demo ()
  (run *uid-demo*))
