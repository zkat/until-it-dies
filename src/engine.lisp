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
  ((initializedp :initform nil :accessor initializedp)
   (clock :accessor clock :initarg :clock)
   (windows :initform (list (make-instance 'window)) :accessor windows :initarg :windows)
   (path :accessor path :initform *default-pathname-defaults* :initarg :path)))

(defmethod initialize-instance :after ((engine engine) &key fps-limit)
  (setf (clock engine)
        (make-instance 'clock :fps-limit fps-limit)))

(defmethod on-draw ((engine engine))
  (loop for window in (windows engine)
     when (openp window)
     do (on-draw window)))

(defmethod on-update ((engine engine) dt)
  (loop for window in (windows engine)
     when (openp window)
     do (on-update window dt)))

(defgeneric step-engine (engine)
  (:method :before ((engine engine))
    (tick (clock engine)))
  (:method ((engine engine))
    (on-update engine (time-delta (clock engine)))
    (on-draw engine)))

(defmethod init :before ((engine engine))
  (setf cl-opengl-bindings:*gl-get-proc-address* #'glop:gl-get-proc-address)
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
