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
	0 ; protection for when sdl-get-ticks wraps
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
  (gl:ortho 0 width 0 height 10 0) ;0,0 is at bottom left of screen. Much nicer for maths.
  (gl:matrix-mode :modelview))

