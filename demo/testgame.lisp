(defpackage uid-demo
  (:use :cl :sheeple :until-it-dies :until-it-dies.demo.resource-info)
  (:shadow :speed)
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

(defparameter *bah* (create-sound (merge-pathnames "sample.wav" *resource-directory*)))

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

(defun angle->color (angle)
  "Turns an angle, in radians, into a color, using the following algorithm:
  0° => 100% red
120° => 100% green
240° => 100% blue
  Angles in between are blended smoothly; for example, 90° is #x3FBF00"
  ;; The following assumes that angle has been produced by `cl:atan', and
  ;; depends heavily upon that and the gory details of the function.
  (flet ((ndist (a b) (abs (- a b))))
    (make-color :r (- 1 (/ (ndist angle 0) pi 2/3))
                :g (- 1 (/ (ndist (mod angle (* 2 pi)) (* pi 2/3)) pi 2/3))
                :b (- 1 (/ (ndist (mod angle (* 2 pi)) (* pi 4/3)) pi 2/3)))))

(defreply draw ((thing *circle*) &key)
  (with-properties (x y radius dx/dt dy/dt) thing
    (flet ((ndist (a b) (abs (- a b))))
      (let* ((speed (sqrt (+ (* dx/dt dx/dt) (* dy/dt dy/dt))))
             (brightness (max (/ speed 4) 0.3))
             (color (angle->color (atan dy/dt dx/dt))))
        (with-properties (red green blue) color
          (setf red (* red brightness)
                green (* green brightness)
                blue (* blue brightness)))
        (with-color color
          (draw-circle (make-point x y) radius :filledp t))))))

(defreply update ((thing *circle*) dt &key)
  (declare (ignore dt))
  (with-properties ((x mouse-x) (y mouse-y)) *uid-demo*
    (with-properties (dx/dt dy/dt (cx x) (cy y)) *circle*
      (macrolet ((apply-velocity (coordinate velocity bound)
                   `(progn (when (> (abs ,velocity) 5)
                             (setf ,velocity (* 0.9 ,velocity)))
                           (unless (< 0 ,coordinate ,bound)
                             (setf ,velocity (- ,velocity)
                                   ,coordinate (if (plusp ,coordinate) ,bound 0)))
                           (incf ,coordinate ,velocity))))
        (apply-velocity cx dx/dt (window-width *uid-demo*))
        (apply-velocity cy dy/dt (window-height *uid-demo*)))
      ;; This is _basically_ Newton's Law of Universal Gravitation
      (let* ((x-gap (- x cx)) (y-gap (- y cy))
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
    (draw *alien* :rotation 45)))

(defreply mouse-move :after ((engine *uid-demo*) x y)
  (with-properties ((alien-x x) (alien-y y)) *alien*
    (setf alien-x x alien-y y)))

(defreply mouse-down ((engine *uid-demo*) button)
  (with-properties (visiblep) *alien*
    (case button
      (0 (setf visiblep (not visiblep)))
      (1 (play *bah*)))))

(defun run-demo ()
  (run *uid-demo*))
