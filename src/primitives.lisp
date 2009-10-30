;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; primitives.lisp
;;;;
;;;; Drawing of basic OGL primitives
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defun make-point (&key (x 0) (y 0) (z 0))
  "For efficiency, points are vectors using the format #(x y z)"
  (vector x y z))

(declaim (inline set-point draw-rectangle draw-triangle draw-point draw-line draw-polygon))
(defun set-point (point)
  (gl:vertex (aref point 0)
             (aref point 1)
             (aref point 2)))

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

(defun draw-triangle (p1 p2 p3 &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :triangles
    (mapc (lambda (point)
            (set-point point))
          (list p1 p2 p3)))
  (when color
    (bind-color *color*)))

(defun draw-quad (p1 p2 p3 p4 &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :quads
    (mapc (lambda (point)
            (set-point point))
          (list p1 p2 p3 p4)))
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
    (mapc (lambda (point)
            (set-point point))
          (list p1 p2)))
  (when color
    (bind-color *color*)))

(defun draw-polygon (points-list &key color)
  (when color
    (bind-color color))
  (gl:with-primitives :polygon
    (mapc (lambda (point)
            (set-point point))
          points-list))
  (when color
    (bind-color *color*)))
