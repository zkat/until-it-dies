(in-package :until-it-dies)

(defun now ()
  (sdl:sdl-get-ticks))

(defun time-difference (time-before)
  "Checks the difference between the internal-time provided and the current time.
Returns both the difference in time and the current-time used in the computation"
  (let ((time-now (now)))
    (values (- time-now time-before)
	    time-now)))

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue")))

(defun degrees-to-radians (degrees)
  (* (/ degrees 180) pi))

(defun radians-to-degrees (radians)
  (/ (* radians 180) pi))