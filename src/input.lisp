;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; input.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Callbacks for GLFW events
;;;
(cffi:defcallback key-hook :void ((key :int) (action uid-glfw:key/button-state))
  "Invokes KEY-DOWN or KEY-UP on the active engine, for control keys."
  (unless (<= key 255)
    (continuable
      (funcall (case action
                 (:press 'key-down)
                 (:release 'key-up))
               *engine* (translate-glfw-control-key key)))))

(cffi:defcallback char-hook :void ((key :int) (action uid-glfw:key/button-state))
  "Invokes KEY-DOWN or KEY-UP on the active engine, for character input."
  (continuable
    (funcall (case action
               (:press 'key-down)
               (:release 'key-up))
             *engine* (code-char key))))

;;;
;;; Joystick support
;;;

;;; low-level stuff
(defun glfw-available-joysticks ()
  (loop for i below 16
     for joystick-present-p = (uid-glfw:get-joystick-param i :present)
     when (= 1 joystick-present-p)
     collect i))

(defun glfw-joystick-num-buttons (joystick-number)
  (uid-glfw:get-joystick-param joystick-number :buttons))

(defun glfw-joystick-num-axes (joystick-number)
  (uid-glfw:get-joystick-param joystick-number :axes))

(defun glfw-joystick-axis-positions (joystick-number num-axes)
  (uid-glfw:get-joystick-pos joystick-number num-axes))

(defun glfw-joystick-button-states (joystick-number num-buttons)
  (uid-glfw:get-joystick-buttons joystick-number num-buttons))

;;; Interface
(defproto =joystick= ()
  ((joystick-number 0)
   (num-axes 0) (num-buttons 0)
   (axis-positions nil) (button-states nil)))

(defun build-joystick (joynum)
  (let ((num-axes (glfw-joystick-num-axes joynum))
        (num-buttons (glfw-joystick-num-buttons joynum)))
    (defobject =joystick=
        ((joystick-number joynum)
         (num-axes num-axes)
         (num-buttons num-buttons)
         (axis-positions (glfw-joystick-axis-positions joynum num-axes))
         (button-states (glfw-joystick-button-states joynum num-buttons))))))

(defun available-joysticks ()
  (loop for joystick-num in (glfw-available-joysticks)
     collect (build-joystick joystick-num)))
