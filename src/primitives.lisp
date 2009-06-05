;; This file is part of Until It Dies

;; primitives.lisp
;;
;; Drawing of basic OGL primitives
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defun rectangle (x y width height &key (z 0) (u1 0) (v1 0) (u2 1) (v2 1))
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

(defsheep =point= ()
  ((x 0)
   (y 0)
   (z 0)))

(defun make-point (&key (x 0) (y 0) (z 0))
  (clone (=point=) ((x x) (y y) (z z))))

(defun draw-rectangle (x y width height &key (color =white=) (z 0))
  (with-properties (r g b a) color
    (gl:color r g b a))
  (gl:with-primitives :quads
      (let* ((w/2 (/ width 2.0))
         (h/2 (/ height 2.0))
         (x1 (- x w/2))
         (x2 (+ x w/2))
         (y1 (+ y h/2))
         (y2 (- y h/2)))
    (gl:vertex x1 y1 z)
    (gl:vertex x2 y1 z)
    (gl:vertex x2 y2 z)
    (gl:vertex x1 y2 z))))

(defun draw-triangle (p1 p2 p3 &key (color =white=))
  (with-properties (r g b a) color
    (gl:color r g b a))
  (gl:with-primitives :triangles
    (mapc (lambda (point)
	    (with-properties (x y z) point
	      (gl:vertex x y z)))
	  (list p1 p2 p3))))

(defun draw-quad (p1 p2 p3 p4 &key (color =white=))
  (with-properties (r g b a) color
    (gl:color r g b a))
  (gl:with-primitives :quads
    (mapc (lambda (point)
	    (with-properties (x y z) point
	      (gl:vertex x y z)))
	  (list p1 p2 p3 p4))))

(defun draw-point (point &key (color =white=))
  (with-properties (r g b a) color
    (gl:color r g b a))
  (gl:with-primitives :points
    (with-properties (x y z) point
      (gl:vertex x y z))))

(defun draw-line (p1 p2 &key (color =white=))
  (with-properties (r g b a) color
    (gl:color r g b a))
  (gl:with-primitives :lines
    (mapc (lambda (point)
	    (with-properties (x y z) point
	      (gl:vertex x y z)))
      (list p1 p2))))

(defun draw-polygon (points-list &key (color =white=))
  (with-properties (r g b a) color
    (gl:color r g b a))
  (gl:with-primitives :polygon
    (mapc (lambda (point)
	    (with-properties (x y z) point
	      (gl:vertex x y z)))
	  points-list)))