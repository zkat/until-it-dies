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

(defun string-join (joiner &rest strings)
  (format nil (format nil "~~{~~A~~^~A~~}" joiner) strings))

;;;
;;; Function operations
;;;

(defun make-gensym-list (length &optional (x "G"))
  "Returns a list of LENGTH gensyms, each generated as if with a call to MAKE-GENSYM,
using the second (optional, defaulting to \"G\") argument."
  (let ((g (if (typep x '(integer 0)) x (string x))))
    (loop repeat length
          collect (gensym g))))

(declaim (inline ensure-function))	; to propagate return type.

(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(define-modify-macro ensure-functionf/1 () ensure-function)

(defmacro ensure-functionf (&rest places)
  `(progn ,@(mapcar (lambda (x) `(ensure-functionf/1 ,x)) places)))

(defun curry (function &rest arguments)
  "Returns a function that applies ARGUMENTS and the arguments
it is called with to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      ;; Using M-V-C we don't need to append the arguments.
      (multiple-value-call fn (values-list arguments) (values-list more)))))

(define-compiler-macro curry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "CURRY")))
    `(let ,(mapcar #'list curries arguments)
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (lambda (&rest more)
         (apply ,function ,@curries more)))))

(defun rcurry (function &rest arguments)
  "Returns a function that applies the arguments it is called
with and ARGUMENTS to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((fn (ensure-function function)))
    (lambda (&rest more)
      (declare (dynamic-extent more))
      (multiple-value-call fn (values-list more) (values-list arguments)))))