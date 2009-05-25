(in-package :until-it-dies)

;;;
;;; Time
;;;
(defun now ()
  (sdl:sdl-get-ticks))

(defun time-difference (time-before)
  "Checks the difference between the internal-time provided and the current time.
Returns both the difference in time and the current-time used in the computation"
  (let* ((time-now (now))
	 (difference (- time-now time-before)))
    (if (minusp difference)
	(time-difference time-now) ; protection for when sdl-get-ticks wraps
	(values (- time-now time-before)
		time-now))))

;;;
;;; Restarting and interactivity
;;;
(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue")))

;;;
;;; Maths
;;;
(defun degrees->radians (degrees)
  (* (/ degrees 180) pi))

(defun radians->degrees (radians)
  (/ (* radians 180) pi))

;;;
;;; OpengGL utils
;;;
(defun setup-ortho-projection (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (gl:ortho 0 width 0 height 0 100) ;0,0 is at bottom left of screen. Much nicer for maths.
  (gl:matrix-mode :modelview))

(defun rectangle (x y width height &key (z 0) (u1 0) (v1 0) (u2 1) (v2 1))
  (let* ((w/2 (/ width 2.0))
         (h/2 (/ height 2.0))
         (x1 (- x w/2))
         (x2 (+ x w/2))
         (y1 (- y h/2))
         (y2 (+ y h/2)))
    (gl:tex-coord u1 v2)
    (gl:vertex x1 y1 z)
    (gl:tex-coord u2 v2)
    (gl:vertex x2 y1 z)
    (gl:tex-coord u2 v1)
    (gl:vertex x2 y2 z)
    (gl:tex-coord u1 v1)
    (gl:vertex x1 y2 z)))