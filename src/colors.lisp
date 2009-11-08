;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; colors.lisp
;;;;
;;;; Color abstraction
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defproto =color= ()
  ((r 1)
   (g 1)
   (b 1)
   (a 1))
  (:documentation
   "A color is an object that represents a certain RGBA value.
The values are used directly by opengl, and should range between 0 and 1 (instead of 0-255)"))
(defun make-color (&key (r 1) (g 1) (b 1) (a 1))
  (defobject =color= ((r r) (g g) (b b) (a a))))

;; Some standard colors
(defproto *black* (=color=)
  ((r 0) (g 0) (b 0)))
(defproto *white* (=color=)
  ((r 1) (g 1) (b 1)))
(defproto *magenta* (=color=)
  ((r 1) (g 0) (b 1)))
(defproto *red* (=color=)
  ((r 1) (g 0) (b 0)))
(defproto *green* (=color=)
  ((r 0) (g 1) (b 0)))
(defproto *blue* (=color=)
  ((r 0) (g 0) (b 1)))
(defproto *yellow* (=color=)
  ((r 1) (g 1) (b 0)))
(defproto *orange* (=color=)
  ((r 1) (g 0.4) (b 0)))
(defproto *brown* (=color=)
  ((r 0.34) (g 0.165) (b 0.165)))

(defvar *color* *white*)
(defun mix-colors (&rest colors)
  (declare (dynamic-extent colors))
  (cond ((null colors)
         nil)
        ((= 1 (length colors))
         (car colors))
        (t (reduce #'%mix-colors colors))))

(defun %mix-colors (color1 color2)
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

(defun color-equal (color1 color2)
  (with-properties ((r1 r) (g1 g) (b1 b) (a1 a)) color1
    (with-properties ((r2 r) (g2 g) (b2 b) (a2 a)) color2
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
  (with-properties (r g b a) color
    (gl:color r g b a)))
