(defpackage #:until-it-dies.examples.text
  (:use :cl)
  (:nicknames :uid.ex.text))
(in-package :uid.ex.text)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defclass text-engine (uid:engine)
  ())
(defclass text-window (uid:window)
  ())

(defparameter *our-font* (make-instance 'uid:font
                                        :filepath (merge-pathnames "example.ttf" *resource-directory*)
                                        :size 14))

(defparameter *string-to-draw*
  (make-instance 'uid:text :string
                 "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))

(defmethod uid:on-draw ((window text-window))
  (uid:set-gl-window window)
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (uid:with-color uid:*black*
   (uid:with-font *our-font*

     (uid:draw "Some text: " :x 150 :y 350)

     (uid:draw-at 10 50 *string-to-draw*
                  :align :left :valign :middle
                  :width 400 :height 400)))
  
  (uid:swap-buffers window))

(defparameter *engine* (make-instance 'text-engine))

(defun run ()
  (setf (uid:windows *engine*) (list (make-instance 'text-window
                                                    :width 400
                                                    :height 400)))
  (uid:run *engine*))