;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; input.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Callbacks for GLFW events
;;;
(cffi:defcallback key-hook :void ((key :int) (action :int))
  "Invokes KEY-DOWN or KEY-UP on the active engine, for control keys."
  (unless (<= key 255)
    (continuable
      (funcall (case action
                 (#.uid-glfw:+press+ 'key-down)
                 (#.uid-glfw:+release+ 'key-up))
               *engine* (translate-glfw-control-key key)))))

(cffi:defcallback char-hook :void ((key :int) (action :int))
  "Invokes KEY-DOWN or KEY-UP on the active engine, for character input."
  (continuable
    (funcall (case action
               (#.uid-glfw:+press+ 'key-down)
               (#.uid-glfw:+release+ 'key-up))
             *engine* (code-char key))))

;;;
;;; Control key translation
;;;
(defun glfw-vector-index (keycode)
  ;; This, like some of the following, assumes the input is valid
  (- keycode uid-glfw:+key-special+ 1))

(defvar *glfw-control-keys*
  (make-array (1+ (glfw-vector-index uid-glfw:+key-last+)) :element-type 'keyword)
  "A vector translating the GLFW control key constants to CL keywords")

(defun glfw-keysym-p (symbol)
  (with-accessors ((name symbol-name) (value symbol-value)) symbol
    (and (constantp symbol)             ; <- CLHS that. I love lisp.
         (= 5 (mismatch "+KEY-" name)) ; Is it key-related?
         (or (< uid-glfw:+key-special+ value uid-glfw:+key-last+)
             ;; These bounds aren't too tight... :(
             (and (= value uid-glfw:+key-last+)
                  (not (eq symbol 'uid-glfw:+key-last+)))))))

(defun translate-glfw-keysym-name (symbol)
  (let ((name (subseq (string-trim "+" (symbol-name symbol)) 4)))
    (cond                  ; This function assumes the input is valid!
      ;; This turns KP-DIVIDE into KEYPAD-DIVIDE
      ((= 3 (mismatch "KP-" name))
       (format nil "KEYPAD-~A" (subseq name 3)))
      ;; This turns RCTRL into RIGHT-CONTROL
      ((some (fun (search _ name)) '("ALT" "CTRL" "SHIFT"))
       (format nil "~:[RIGHT~;LEFT~]-~A"
               (char= (char name 0) #\L) (subseq name 1)))
      ;; This turns PAGEUP to PAGE-UP
      ((= 4 (mismatch "PAGE" name))
       (format nil "PAGE-~A" (subseq name 4)))
      ;; Now we get a bit crappy and hardcoded
      ((string= "DEL" name) "DELETE")
      ((string= "ESC" name) "ESCAPE")
      ;; If it doesn't match any special case, just return the name as-is
      (t name))))

(do-external-symbols (symbol :uid-glfw)
  (with-accessors ((name symbol-name) (value symbol-value)) symbol
    (when (glfw-keysym-p symbol)
      (setf (svref *glfw-control-keys* (glfw-vector-index value))
            (intern (translate-glfw-keysym-name symbol) :keyword)))))

(defun translate-glfw-control-key (keycode)
  (svref *glfw-control-keys* (glfw-vector-index keycode)))

;;;
;;; Joystick support
;;;

;;; low-level stuff
(defun glfw-available-joysticks ()
  (loop for i below 16 
     for joystick-present-p = (uid-glfw:get-joystick-param i uid-glfw:+present+)
     when (= 1 joystick-present-p)
     collect i))

(defun glfw-joystick-num-buttons (joystick-number)
  (uid-glfw:get-joystick-param joystick-number uid-glfw:+buttons+))

(defun glfw-joystick-num-axes (joystick-number)
  (uid-glfw:get-joystick-param joystick-number uid-glfw:+axes+))

(defun glfw-joystick-axis-positions (joystick-number num-axes)
  (uid-glfw:get-joystick-pos joystick-number num-axes))

(defun glfw-joystick-button-states (joystick-number num-buttons)
  (loop for state in (uid-glfw:get-joystick-buttons joystick-number num-buttons)
     collect (if (= state uid-glfw:+press+) :pressed :released)))

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

