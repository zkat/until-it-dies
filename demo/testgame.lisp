(defpackage uid-demo
  (:use :cl :sheeple)
  (:shadow :speed)
  (:export :run-demo))
(in-package :uid-demo)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defproto =uid-demo= uid:=engine=
  ((uid:title "UID Demo")
   (uid:resizablep nil)
   (uid:window-width 400)
   (uid:window-height 400)))

(defproto =game-object= ()
  ((x 0)
   (y 0)
   content))

(defreply uid:draw ((thing =game-object=) &rest args &key)
  (with-properties (x y content) thing
    (apply 'uid:draw-at x y content args)))

(defreply uid:update ((thing =game-object=) dt &rest args &key)
  (apply 'uid:update (content thing) dt args))

;; We use defproto here for convenience, but keep our *earmuffs*
(defproto *alien* =game-object=
  ((content (uid:create-image (merge-pathnames "lisplogo_alien_256.png" *resource-directory*)))
   visiblep (x 255) (y 356)))

(defparameter *bah* (uid:create-sound (merge-pathnames "sample.wav" *resource-directory*)))

(defreply uid:draw :around ((thing *alien*) &key)
  (with-properties (visiblep) thing
    (when visiblep (call-next-reply))))

(defproto *anim* =game-object=
  ((content (uid:create-animation (merge-pathnames "explosion.png" *resource-directory*) 15 14 0.05 14))
   (speed 300) (x 50) (y 50)))

(defproto *our-font* uid:=font=
  ((uid:filepath (merge-pathnames "example.otf" *resource-directory*))))

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
    (uid:make-color :r (- 1 (/ (ndist angle 0) pi 2/3))
                    :g (- 1 (/ (ndist (mod angle (* 2 pi)) (* pi 2/3)) pi 2/3))
                    :b (- 1 (/ (ndist (mod angle (* 2 pi)) (* pi 4/3)) pi 2/3)))))

(defreply uid:draw ((thing *circle*) &key)
  (with-properties (x y radius dx/dt dy/dt) thing
    (flet ((ndist (a b) (abs (- a b))))
      (let* ((speed (sqrt (+ (* dx/dt dx/dt) (* dy/dt dy/dt))))
             (brightness (max (/ speed 4) 0.3))
             (color (angle->color (atan dy/dt dx/dt))))
        (with-properties (uid:red uid:green uid:blue) color
          (setf uid:red (* uid:red brightness)
                uid:green (* uid:green brightness)
                uid:blue (* uid:blue brightness)))
        (uid:with-color color
          (uid:draw-circle (uid:make-point x y) radius :filledp t))))))

(defreply uid:update ((thing *circle*) dt &key)
  (declare (ignore dt))
  (with-properties ((x uid:mouse-x) (y uid:mouse-y)) =uid-demo=
    (with-properties (dx/dt dy/dt (cx x) (cy y)) *circle*
      (macrolet ((apply-velocity (coordinate velocity bound)
                   `(progn (when (> (abs ,velocity) 5)
                             (setf ,velocity (* 0.9 ,velocity)))
                           (unless (< 0 ,coordinate ,bound)
                             (setf ,velocity (- ,velocity)
                                   ,coordinate (if (plusp ,coordinate) ,bound 0)))
                           (incf ,coordinate ,velocity))))
        (apply-velocity cx dx/dt (uid:window-width =uid-demo=))
        (apply-velocity cy dy/dt (uid:window-height =uid-demo=)))
      ;; This is _basically_ Newton's Law of Universal Gravitation
      (let* ((x-gap (- x cx)) (y-gap (- y cy))
             (accel (/ (+ (expt x-gap 2) (expt y-gap 2)))))
        (incf dx/dt (* accel x-gap))
        (incf dy/dt (* accel y-gap))))))

(defreply uid:update ((engine =uid-demo=) dt &key)
  (uid:update *anim* dt)
  (uid:update *circle* dt)
  (with-properties (x y speed) *anim*
    (when (and (uid:key-down-p :right)
               (< x (uid:window-width engine)))
      (incf x (* speed dt)))
    (when (and (uid:key-down-p :left)
               (< 0 x))
      (decf x (* speed dt)))
    (when (and (uid:key-down-p :up)
               (< y (uid:window-height engine)))
      (incf y (* speed dt)))
    (when (and (uid:key-down-p :down)
               (< 0 y))
      (decf y (* speed dt)))))

(defparameter *dot-color* (uid:mix-colors uid:*white* uid:*black* uid:*blue* uid:*green*))
(defreply uid:draw ((engine =uid-demo=) &key)
  (let ((scale-factor 4))
    (uid:with-color *dot-color*
      (uid:draw-points (loop for i below 100 collect (uid:make-point (random (uid:window-width engine))
                                                                     (random (uid:window-height engine))))))
    (uid:with-font *our-font*
      (uid:draw-at 45 20 "Try left-clicking, right-clicking, and pressing the arrow keys!"))
    (uid:draw *anim* :x-scale scale-factor :y-scale scale-factor)
    (uid:draw *circle*)
    (uid:draw *alien* :rotation 45)))

(defreply uid:mouse-move :after ((engine =uid-demo=) x y)
  (with-properties ((alien-x x) (alien-y y)) *alien*
    (setf alien-x x alien-y y)))

(defreply uid:mouse-down ((engine =uid-demo=) button)
  (with-properties (visiblep) *alien*
    (case button
      (0 (setf visiblep (not visiblep)))
      (1 (uid:play *bah*)))))

(defreply uid:joystick-button-down ((engine =uid-demo=) joystick button)
  (when (= 0 (uid:joystick-number joystick))
    (case button
      (0 (uid:play *bah*)))))

(defreply uid:joystick-move ((engine =uid-demo=) joystick axis state)
  (format t "Joystick: ~A Axis: ~A Position: ~A~%" joystick axis state))

(defun run-demo ()
  (uid:run =uid-demo=))
