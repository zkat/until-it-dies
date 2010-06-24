;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; engine.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Basic engine
;;;
(defclass base-engine ()
  ((initializedp :initform nil :accessor initializedp)))

(defgeneric step-engine (engine))

(defmethod init ((engine base-engine))
  engine)

(defmethod init :after ((engine base-engine))
  (setf (initializedp engine) t))

(defmethod teardown ((engine base-engine))
  engine)

(defmethod teardown :after ((engine base-engine))
  (setf (initializedp engine) nil))

(defmethod run :around ((engine base-engine))
  (unwind-protect
       (progn (init engine)
              (call-next-method))
    (teardown engine))
  engine)
