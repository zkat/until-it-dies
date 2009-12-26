;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; colors.lisp
;;;;
;;;; Color abstraction
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defproto =color= ()
  ((red 1)
   (green 1)
   (blue 1)
   (alpha 1))
  :documentation
  "A color is an object that represents a certain RGBA value.
The values are used directly by opengl, and should range between 0 and 1 (instead of 0-255)")
(defun make-color (&key (r 1) (g 1) (b 1) (a 1) (name nil name-supplied-p))
  ;; Ugly hack needed because of defobject's macroness
  (if name-supplied-p
      (defobject =color= ((red r) (green g) (blue b) (alpha a)) :nickname name)
      (defobject =color= ((red r) (green g) (blue b) (alpha a)))))

;; Some standard colors
(defproto *black* (=color=)
  ((red 0) (green 0) (blue 0)))
(defproto *white* (=color=)
  ((red 1) (green 1) (blue 1)))
(defproto *magenta* (=color=)
  ((red 1) (green 0) (blue 1)))
(defproto *red* (=color=)
  ((red 1) (green 0) (blue 0)))
(defproto *green* (=color=)
  ((red 0) (green 1) (blue 0)))
(defproto *blue* (=color=)
  ((red 0) (green 0) (blue 1)))
(defproto *yellow* (=color=)
  ((red 1) (green 1) (blue 0)))
(defproto *orange* (=color=)
  ((red 1) (green 0.4) (blue 0)))
(defproto *brown* (=color=)
  ((red 0.34) (green 0.165) (blue 0.165)))

(defvar *color* *white*)
(defun mix-colors (&rest colors)
  (declare (dynamic-extent colors))
  (cond ((null colors)
         nil)
        ((= 1 (length colors))
         (car colors))
        (t (reduce #'%mix-colors colors))))

(defun %mix-colors (color1 color2)
  (let* ((r1 (red color1))
         (g1 (green color1))
         (b1 (blue color1))
         (a1 (alpha color1))
         (r2 (red color2))
         (g2 (green color2))
         (b2 (blue color2))
         (a2 (alpha color2)))
    (make-color :r (/ (+ r1 r2) 2)
                :g (/ (+ g1 g2) 2)
                :b (/ (+ b1 b2) 2)
                :a (/ (+ a1 a2) 2))))

(defun color-equal (color1 color2)
  (with-properties ((r1 red) (g1 green) (b1 blue) (a1 alpha)) color1
    (with-properties ((r2 red) (g2 green) (b2 blue) (a2 alpha)) color2
      (when (and (= r1 r2) (= g1 g2) (= b1 b2) (= a1 a2)) t))))

(defmacro with-color (color &body body)
  (let ((color-name (gensym "COLOR-"))
        (rebindp (gensym "REBINDP-")))
    `(let* ((,color-name ,color)
            (,rebindp (not (or (null ,color-name)
                               (eq *color* ,color-name)
                               (color-equal *color* ,color-name)))))
       (unwind-protect
            (let ((*color* ,color))
              (when ,rebindp
                (bind-color *color*))
              ,@body)
         (when ,rebindp (bind-color *color*))))))

(defun bind-color (color)
  (with-properties (red green blue alpha) color
    (gl:color red green blue alpha)))
