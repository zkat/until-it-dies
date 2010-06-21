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
  (flet ((average (x y)
           (/ (+ x y) 2)))
    (sequence->color (map 'vector #'average (color->vector color1) (color->vector color2)))))

(defun color-equal (color1 color2)
  (equal (color->vector color1) (color->vector color2)))

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

(defun color->vector (color)
  (vector (color-red color) (color-green color) (color-blue color) (color-alpha color)))

(defun color->list (color)
  (list (color-red color) (color-green color) (color-blue color) (color-alpha color)))

(defun sequence->color (sequence)
  (make-color :red (elt sequence 0)
              :green (elt sequence 1)
              :blue (elt sequence 2)
              :alpha (elt sequence 3)))

(defun bind-color (color)
  (apply #'gl:color (color->list color)))

