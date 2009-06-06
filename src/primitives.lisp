;; This file is part of Until It Dies

;; primitives.lisp
;;
;; Drawing of basic OGL primitives
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(declaim (optimize (speed 3)))
;;;
;;; Colors
;;;
(defstruct (color (:type vector))
  ;; A color is an object that represents a certain RGBA value. 
  ;; The values are used directly by opengl, and should range between 0 and 1 (instead of 0-255)"
  (r 1)
  (g 1)
  (b 1)
  (a 1))
(defun r (color)
  (color-r color))
(defun g (color)
  (color-g color))
(defun b (color)
  (color-b color))
(defun a (color)
  (color-a color))

;; Some standard colors
(defparameter *black*
  (make-color :r 0 :g 0 :b 0))
(defparameter *white*
  (make-color :r 1 :g 1 :b 1))
(defparameter *magenta*
  (make-color :r 1 :g 0 :b 1))
(defparameter *red*
  (make-color :r 1 :g 0 :b 0))
(defparameter *green*
  (make-color :r 0 :g 1 :b 0))
(defparameter *blue*
  (make-color :r 0 :g 0 :b 1))
(defparameter *yellow*
  (make-color :r 1 :g 1 :b 0))
(defparameter *orange*
  (make-color :r 1 :g 0.40 :b 0))
(defparameter *brown*
  (make-color :r 0.34 :g 0.165 :b 0.165))

(defun mix-colors (color1 color2)
  (let* ((r1 (r color1))
	 (g1 (g color1))
	 (b1 (b color1))
	 (a1 (a color1))
	 (r2 (r color2))
	 (g2 (g color2))
	 (b2 (b color2))
	 (a2 (a color2)))
      (make-color :r (/ (+ r1 r2) 2)
		  :g (/ (+ g1 g2) 2)
		  :b (/ (+ b1 b2) 2)
		  :a (/ (+ a1 a2) 2))))

(defun bind-color (color)
  (with-accessors ((r r)
		   (g g)
		   (b b)
		   (a a))
      color
    (gl:color r g b a)))

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

(defun make-point (&key (x 0) (y 0) (z 0))
  "For efficiency, points are vectors using the format #(x y z)"
  (vector x y z))

(declaim (inline set-point draw-rectangle draw-triangle draw-point draw-line draw-polygon))
(defun set-point (point)
  (gl:vertex (aref point 0)
             (aref point 1)
             (aref point 2)))

(defun draw-rectangle (x y width height &key (color *white*) (z 0))
  (bind-color color)
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

(defun draw-triangle (p1 p2 p3 &key (color *white*))
  (bind-color color)
  (gl:with-primitives :triangles
    (mapc (lambda (point)
            (set-point point))
	  (list p1 p2 p3))))

(defun draw-quad (p1 p2 p3 p4 &key (color *white*))
  (bind-color color)
  (gl:with-primitives :quads
    (mapc (lambda (point)
            (set-point point))
	  (list p1 p2 p3 p4))))

(defun draw-point (point &key (color *white*))
  (bind-color color)
  (gl:with-primitives :points
    (set-point point)))

(defun draw-line (p1 p2 &key (color *white*))
  (bind-color color)
  (gl:with-primitives :lines
    (mapc (lambda (point)
            (set-point point))
      (list p1 p2))))

(defun draw-polygon (points-list &key (color *white*))
  (bind-color color)
  (gl:with-primitives :polygon
    (mapc (lambda (point)
            (set-point point))
	  points-list)))
