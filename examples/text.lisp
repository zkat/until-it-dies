(defpackage #:until-it-dies.examples.text
  (:use :cl)
  (:export :run))
(in-package #:until-it-dies.examples.text)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defclass text-example (uid:simple-game-engine)
  ()
  (:default-initargs :title "UID Text Rendering Example"
    :clear-color uid:*white*))

(defparameter *game* (make-instance 'text-example))

(defparameter *our-font* (make-instance 'uid::ftgl-font
                                        :filepath (merge-pathnames "example.ttf" *resource-directory*)
                                        :size 14))

(defparameter *string-to-draw*
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defmethod uid:on-draw ((game text-example))
  (uid:clear game)

  (uid:with-color uid:*black*
    (uid:with-font *our-font*

      (uid:draw (format nil "FPS: ~,2f" (uid:fps (uid:clock game)))
                :x 0 :y 0)

      (uid:draw "Some text: " :x 150 :y 350)

      (uid:draw-at 20 300 *string-to-draw*))))

(defun run ()
  (uid:run *game*))