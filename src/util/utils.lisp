;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package :until-it-dies)

(defmacro fun (&body body)
  "This macro puts the FUN back in LAMBDA"
  `(lambda (&optional _)
     (declare (ignorable _))
     ,@body))

;;;
;;; Time
;;;
(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun time-difference (time-before)
  "Checks the difference between the internal-time provided and the current time.
Returns both the difference in time and the current-time used in the computation"
  (let* ((time-now (now))
         (difference (- time-now time-before)))
    (if (minusp difference)
        0                               ; just in case
        (values (- time-now time-before)
                time-now))))

;;;
;;; Restarting and interactivity
;;;
(defmacro continuable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(with-simple-restart (continue "Continue") ,@body))

;;;
;;; Maths
;;;
(defun degrees->radians (degrees)
  (* (/ degrees 180) pi))

(defun radians->degrees (radians)
  (/ (* radians 180) pi))

;;;
;;; String formatting
;;;
(defun out (&rest objects)
  (princ (apply #'build-string objects)))

(defun build-string (&rest objects)
  (declare (list objects))
  (apply #'concatenate 'string
         (map 'list (lambda (obj)
                      (cond ((eq :% obj)
                             (format nil "~%"))
                            ((and (symbolp obj)
                                  (numberp (read-from-string (symbol-name obj))))
                             (apply #'concatenate 'string
                                    (loop repeat (read-from-string (symbol-name obj))
                                       collect " ")))
                            (t (format nil "~A" obj))))
              objects)))

