;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; colors.lisp
;;;;
;;;; Color abstraction
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defstruct color
  (red 1)
  (green 1)
  (blue 1)
  (alpha 1)
  (name ""))

;; Some standard colors
(defmacro defcolor (name red green blue &optional (nickname ""))
  `(defparameter ,name (make-color :red ,red :green ,green :blue ,blue :name ,nickname)))

(defcolor *black* 0 0 0 "black")
(defcolor *white* 1 1 1 "white")
(defcolor *magenta* 1 0 1 "magenta")
(defcolor *red* 1 0 0 "red")
(defcolor *green* 0 1 0 "green")
(defcolor *blue* 0 0 1 "blue")
(defcolor *yellow* 1 1 0 "yellow")
(defcolor *orange* 1 0.4 0 "orange")
(defcolor *brown* 0.34 0.165 0.165 "brown")

(defvar *color* *white*)

(defun mix-colors (&rest colors)
  (declare (dynamic-extent colors))
  (cond ((null colors)
         nil)
        ((= 1 (length colors))
         (car colors))
        (t (reduce #'%mix-colors colors))))

(defun %mix-colors (color1 color2)
  (let* ((r1 (color-red color1))
         (g1 (color-green color1))
         (b1 (color-blue color1))
         (a1 (color-alpha color1))
         (r2 (color-red color2))
         (g2 (color-green color2))
         (b2 (color-blue color2))
         (a2 (color-alpha color2)))
    (make-color :red (/ (+ r1 r2) 2)
                :green (/ (+ g1 g2) 2)
                :blue (/ (+ b1 b2) 2)
                :alpha (/ (+ a1 a2) 2))))

(defun color-equal (color1 color2)
  (with-accessors ((r1 color-red) (g1 color-green) (b1 color-blue) (a1 color-alpha)) color1
    (with-accessors ((r2 color-red) (g2 color-green) (b2 color-blue) (a2 color-alpha)) color2
      (when (and (= r1 r2) (= g1 g2) (= b1 b2) (= a1 a2))
        t))))

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
  (with-accessors ((red color-red) (green color-green)
                   (blue color-blue) (alpha color-alpha))
      color
    (gl:color red green blue alpha)))
