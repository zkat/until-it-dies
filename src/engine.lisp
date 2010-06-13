;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; engine.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Engine Prototype
;;;
(defclass engine ()
  ((last-tick-time :initform (now) :accessor last-tick-time)
   (time-delta :initform 0 :accessor time-delta)
   (initializedp :initform nil :accessor initializedp)
   (windows :initform nil :accessor windows :initarg :windows)))

(defmethod on-draw ((engine engine))
  (loop for window in (windows engine)
     when (openp window)
     do (on-draw window)))

(defmethod on-update ((engine engine) dt)
  (loop for window in (windows engine)
     when (openp window)
     do (on-update window dt)))

(defun update-time (engine)
  (with-accessors ((dt time-delta) (last-time last-tick-time)) engine
    (multiple-value-bind (new-dt now)
        (time-difference last-time)
      (setf last-time now
            dt new-dt)))
  t)

(defgeneric step-engine (engine)
  (:method :before ((engine engine))
    (update-time engine))
  (:method ((engine engine))
    (on-update engine (time-delta engine))
    (on-draw engine)))

(defmethod init :before ((engine engine))
  (setf (last-tick-time engine) (now)
        cl-opengl-bindings:*gl-get-proc-address* #'glop:gl-get-proc-address)
  (mapc #'init (windows engine)))

(defmethod init ((engine engine))
  engine)

(defmethod init :after ((engine engine))
  (setf (initializedp engine) t))

(defmethod teardown ((engine engine))
  engine)


(defmethod teardown :after ((engine engine))
  (mapc #'teardown (windows engine))
  (setf (initializedp engine) nil))

;;;
;;; Main Function Extraordinaire
;;;
(defmethod run ((engine engine))
  (unwind-protect
       (progn (init engine)
              (loop while (every #'openp (windows engine)) do
                   (continuable (step-engine engine))))
    (teardown engine))
  engine)
