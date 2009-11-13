;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; view.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;; View objects
(defproto =view= ()
  (view-left 0) (view-right 1) (view-bottom 0) (view-top 1) 
  (view-far 0) (view-near 10) (view-zoom 1.0))

(defun create-view (x y width height &key (far 0) (near 10))
  (defobject =view= ((view-left x) (view-right (+ x view-width))
                     (view-bottom y) (view-top (+ y height))
                     (view-far far) (view-near near))))

(defreply width ((view =view=))
  (- (view-right view) (view-left view)))

(defreply (setf width) (new-value (view =view=))
  (setf (view-right view) (+ (view-left view) new-value)))

(defreply height ((view =view=))
  (- (view-top view) (view-bottom view)))

(defreply (setf height) (new-value (view =view))
  (setf (view-top view) (+ (view-bottom view) new-value)))

(defmessage zoom-view (view zoom-factor)
  (:reply ((view =view=) zoom-factor)
    (let ((width-diff (* zoom-factor (width view)))
          (height-diff (* zoom-factor (height view))))
      (with-properties (view-left view-right view-bottom view-top view-zoom)
          view
        (decf view-left width-diff)
        (incf view-right width-diff)
        (decf view-bottom height-diff)
        (incf view-top height-diff)
        (incf view-zoom zoom-factor)))))

(defmessage move-view (view dx dy)
  (:reply ((view =view=) dx dy)
    (with-properties (view-left view-right view-bottom view-top) 
        view
      (incf view-left dx) (incf view-right dx)
      (incf view-bottom dy) (incf view-top dy))))

(defmessage update-view (view x y width height &key far near)
  (:reply ((view =view=) x y width height)
    (with-properties (view-left view-right view-bottom view-top view-far view-near)
        view
      (setf view-left x
            view-right (+ x width)
            view-bottom y
            view-top (+ y height))
      (when far (setf view-far far))
      (when near (setf view-near near)))))

(defmessage set-view (view)
  (:reply ((view =view=))
    (with-properties (view-left view-right view-top view-bottom view-far view-near)
        view
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho view-left view-right view-bottom view-top view-near view-far)
      (gl:matrix-mode :modelview)
      (gl:load-identity))))
