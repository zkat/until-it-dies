(defpackage #:until-it-dies.examples.image
  (:use :cl)
  (:export :run))
(in-package #:until-it-dies.examples.image)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defclass my-engine (uid:engine)
  ())
(defclass my-window (uid:window)
  ()
  (:default-initargs :clear-color uid:*black*))

(defparameter *image* (make-instance 'uid:image
                                     :texture-filepath
                                     (merge-pathnames "lisplogo_alien_256.png" *resource-directory*)))

(defmethod uid:on-draw ((window my-window))
  (uid:clear window)
  (uid:draw *image* :x 100 :y 100))

(defparameter *engine* (make-instance 'my-engine
                                      :windows
                                      (list (make-instance 'my-window :width 400 :height 400))))

(defun run ()
  (uid:run *engine*))
