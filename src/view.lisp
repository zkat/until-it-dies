;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; view.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;; Views
(defclass view ()
  ((left-edge :initform 0 :accessor left-edge :initarg :left-edge)
   (right-edge :initform 400 :accessor right-edge :initarg :right-edge)
   (bottom-edge :initform 0 :accessor bottom-edge :initarg :bottom-edge)
   (top-edge :initform 400 :accessor top-edge :initarg :top-edge)
   (far-edge :initform 0 :accessor far-edge :initarg :far-edge)
   (near-edge :initform 10 :accessor near-edge :initarg :near-edge)
   (zoom :initform 1.0 :accessor zoom :initarg :zoom)))

(defgeneric view-width (view)
  (:method ((view view))
    (- (right-edge view) (left-edge view))))

(defgeneric (setf view-width) (new-value view)
  (:method (new-value (view view))
    (setf (right-edge view) (+ (left-edge view) new-value))))

(defgeneric view-height (view)
  (:method ((view view))
    (- (top-edge view) (bottom-edge view))))

(defgeneric (setf view-height) (new-value view)
  (:method (new-value (view view))
    (setf (top-edge view) (+ (bottom-edge view) new-value))))

(defgeneric zoom-view (view zoom-factor)
  (:method ((view view) zoom-factor)
    (let ((width-diff (* zoom-factor (view-width view)))
          (height-diff (* zoom-factor (view-height view))))
      (with-accessors ((left-edge left-edge) (right-edge right-edge)
                       (bottom-edge bottom-edge) (top-edge top-edge) (zoom zoom))
          view
        (decf left-edge width-diff)
        (incf right-edge width-diff)
        (decf bottom-edge height-diff)
        (incf top-edge height-diff)
        (incf zoom zoom-factor)))))

(defgeneric move-view (view dx dy)
  (:method ((view view) dx dy)
    (with-accessors ((left-edge left-edge) (right-edge right-edge)
                     (top-edge top-edge) (bottom-edge bottom-edge))
        view
      (incf left-edge dx) (incf right-edge dx)
      (incf bottom-edge dy) (incf top-edge dy))))

(defgeneric update-view (view x y width height &key far near)
  (:method ((view view) x y width height &key far near)
    (with-accessors ((left-edge left-edge) (right-edge right-edge)
                     (bottom-edge bottom-edge) (top-edge top-edge)
                     (far-edge far-edge) (near-edge near-edge))
        view
      (setf left-edge x
            right-edge (+ x width)
            bottom-edge y
            top-edge (+ y height))
      (when far (setf far-edge far))
      (when near (setf near-edge near)))))

(defgeneric set-view (view)
  (:method ((view view))
    (with-accessors ((left-edge left-edge) (right-edge right-edge)
                     (top-edge top-edge) (bottom-edge bottom-edge)
                     (far-edge far-edge) (near-edge near-edge))
        view
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho left-edge right-edge bottom-edge top-edge near-edge far-edge)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (let* ((x-scale (if (> left-edge right-edge) -1 1))
             (x-translate (if (= x-scale 1) 0 (- (- left-edge right-edge))))
             (y-scale (if (> bottom-edge top-edge) -1 1))
             (y-translate (if (= y-scale 1) 0 (- (- bottom-edge top-edge))))
             (z-scale (if (> far-edge near-edge) -1 1))
             (z-translate (if (= z-scale 1) 0 (- (- far-edge near-edge)))))
        (gl:scale x-scale y-scale z-scale)
        (gl:translate x-translate y-translate z-translate)))))
