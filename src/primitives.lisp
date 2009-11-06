;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; primitives.lisp
;;;;
;;;; Drawing of basic OGL primitives
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (deftype point ()
   '(vector real 3))

 (defun make-point (x y &optional (z 0))
   (vector x y z))

 (defun point-x (point)
   (svref point 0))
 (defun point-y (point)
   (svref point 1))
 (defun point-z (point)
   (svref point 2)))

(defun set-point (point)
  (gl:vertex (point-x point) (point-y point) (point-z point)))

(defun draw-rectangle (x y width height &key (z 0) (u1 0) (v1 0) (u2 1) (v2 1) color)
  (when color
    (bind-color color))
  (gl:with-primitives :quads
    (let* ((w/2 (/ width 2.0))
           (h/2 (/ height 2.0))
           (x1 (- x w/2))
           (x2 (+ x w/2))
           (y1 (+ y h/2))
           (y2 (- y h/2)))
      (gl:tex-coord u1 v2)
      (gl:vertex x1 y1 z)
      (gl:tex-coord u2 v2)
      (gl:vertex x2 y1 z)
      (gl:tex-coord u2 v1)
      (gl:vertex x2 y2 z)
      (gl:tex-coord u1 v1)
      (gl:vertex x1 y2 z)))
  (when color
    (bind-color *color*)))

(defun draw-circle (center radius &key (resolution 20) color (filledp t))
  (when color
    (bind-color color))
  (gl:with-primitives (if filledp :triangle-fan :line-loop)
    (loop for angle from 0 to 360 by resolution
       for radian = (degrees->radians angle)
       do (gl:vertex (+ (point-x center) (* radius (cos radian)))
                     (+ (point-y center) (* radius (sin radian)))))))

(defun draw-triangle (p1 p2 p3 &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :triangles
    (set-point p1)
    (set-point p2)
    (set-point p3))
  (when color
    (bind-color *color*)))

(defun draw-quad (p1 p2 p3 p4 &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :quads
    (set-point p1)
    (set-point p2)
    (set-point p3)
    (set-point p4))
  (when color
    (bind-color *color*)))

(defun draw-point (point &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :points
    (set-point point))
  (when color
    (bind-color *color*)))

(defun draw-line (p1 p2 &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :lines
    (set-point p1)
    (set-point p2))
  (when color
    (bind-color *color*)))

(defun draw-polygon (points &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :polygon
    (map nil 'set-point points))
  (when color
    (bind-color *color*)))
