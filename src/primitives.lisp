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

(defun draw-rectangle (x y width height &key (z 0) (u1 0) (v1 0) (u2 1) (v2 1) (color *color*))
  (with-color color
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
        (gl:vertex x1 y2 z)))))

(defun draw-circle (center radius &key (resolution 20) (color *color*) (filledp t))
  (with-color color
    (let* ((theta (* 2 (/ pi resolution)))
           (tangential-factor (tan theta))
           (radial-factor (- 1 (cos theta))))
      (gl:with-primitives (if filledp :triangle-fan :line-loop)
        (loop with x = (+ (point-x center) radius)
           with y = (point-y center)
           repeat resolution
           do (gl:vertex x y (point-z center))
             (let ((tx (- (- y (point-y center))))
                   (ty (- x (point-x center))))
               (incf x (* tx tangential-factor))
               (incf y (* ty tangential-factor)))
             (let ((rx (- (point-x center) x))
                   (ry (- (point-y center) y)))
               (incf x (* rx radial-factor))
               (incf y (* ry radial-factor))))))))

(defun draw-triangle (p1 p2 p3 &key (color *color*))
  (with-color color
    (gl:with-primitives :triangles
      (set-point p1)
      (set-point p2)
      (set-point p3))))

(defun draw-quad (p1 p2 p3 p4 &key (color *color*))
  (with-color color
    (gl:with-primitives :quads
     (set-point p1)
     (set-point p2)
     (set-point p3)
     (set-point p4))))

(defun draw-point (point &key (color *color*))
  (with-color color
    (gl:with-primitives :points
      (set-point point))))

(defun draw-points (points &key (color *color*))
  (with-color color
    (gl:with-primitives :points
      (map nil #'set-point points))))

(defun draw-line (p1 p2 &key (color *color*))
  (with-color color
    (gl:with-primitives :lines
      (set-point p1)
      (set-point p2))))

(defun draw-polygon (points &key (color *color*))
  (with-color color
    (gl:with-primitives :polygon
      (map nil 'set-point points))))
