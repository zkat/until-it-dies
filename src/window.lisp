;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; window.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Abstract, Base window
;;;
(defclass base-window ()
  ((title :initform "UID Window" :accessor title :initarg :title)
   (width :initform 800 :accessor width :initarg :width)
   (height :initform 600 :accessor height :initarg :height)
   (fullscreenp :initform nil :accessor fullscreenp :initarg :fullscreenp)
   (resizablep :initform nil :accessor resizablep :initarg :resizablep)
   (key-repeat-p :initform nil :accessor key-repeat-p)
   (clear-color :initform *black* :accessor clear-color :initarg :clear-color)
   (clear-buffers :initform '(:color-buffer-bit :depth-buffer-bit) :initarg :clear-buffers
                  :accessor clear-buffers)))

(defgeneric (setf height) (new-value window))
(defgeneric (setf width) (new-value window))

(defgeneric set-gl-window (window))

(defgeneric clear (window)
  (:method ((window base-window))
    (apply #'gl:clear-color (color->list (clear-color window)))
    (apply #'gl:clear (clear-buffers window))))

(defgeneric swap-buffers (window))

(defgeneric open-window (window))

(defgeneric close-window (window))

(defmethod init ((window base-window))
  (open-window window))

(defmethod teardown ((window base-window))
  (close-window window))

;; Event API
(macrolet ((defev (name lambda-list &body options)
               `(defgeneric ,name ,lambda-list
                  (:method ,lambda-list
                    (declare (ignore ,@lambda-list))
                    (values))
                  ,@options)))
  (defev on-key-down (window keycode keysym string))
  (defev on-key-up (window keycode keysym string))
  (defev on-mouse-down (window button))
  (defev on-mouse-up (window button))
  (defev on-mouse-move (window x y))
  (defev on-joy-button-down (window joystick button))
  (defev on-joy-button-up (window joystick button))
  (defev on-joy-move (window joystick old-axes new-axes))
  (defev on-resize (window width height))
  (defev on-expose (window))
  (defev on-obscured (window))
  (defev on-unobscured (window))
  (defev on-focus (window))
  (defev on-blur (window))
  (defev on-close (window)))

(defmethod on-update ((window base-window) dt)
  (declare (ignore dt))
  (values))

(defmethod on-draw :before ((window base-window))
  (uid:set-gl-window window))

(defmethod on-draw ((window base-window))
  (values))

(defmethod on-draw :after ((window base-window))
  (uid:swap-buffers window))

(defmethod on-close ((window base-window))
  (teardown window))

;;;
;;; Glop Windows
;;;
(defclass glop-window (base-window)
  ((openp :initform nil :accessor openp)
   (mouse-x :initform 0 :accessor mouse-x)
   (mouse-y :initform 0 :accessor mouse-y)
   (glop-window :initform nil :accessor glop-window)))

(defmethod (setf title) :after (new-value (window glop-window))
  (when (openp window)
    (glop:set-window-title (glop-window window) new-value)))

(defmethod (setf fullscreenp) :after (new-value (window glop-window))
  (when (openp window)
    (glop:set-fullscreen (glop-window window) new-value)))

(defmethod set-gl-window ((window glop-window))
  (glop:set-gl-window (glop-window window))
  window)

(defmethod swap-buffers ((window glop-window))
  (glop:swap-buffers (glop-window window))
  window)

(defmethod open-window ((window glop-window))
  (unless (openp window)
    (setf (glop-window window)
          (glop:create-window (title window)
                              (width window)
                              (height window)
                              :fullscreen (fullscreenp window)
                              :double-buffer t
                              :stencil-buffer t
                              :stencil-size 1
                              :accum-buffer t))
    (set-gl-window window)
    (set-view (view window))
    (setf (openp window) t))
  window)

(defmethod close-window ((window glop-window))
  (when (openp window)
    (glop:destroy-window (glop-window window))
    (setf (openp window) nil))
  window)

(defgeneric dispatch-glop-event (window event))
(macrolet ((dispatch (event-class-name &body body)
             `(defmethod dispatch-glop-event ((window glop-window) (event ,event-class-name))
                ,@body)))

  (dispatch glop:key-press-event
    (on-key-down window (glop:keycode event) (glop:keysym event) (glop:text event)))

  (dispatch glop:key-release-event
    (on-key-up window (glop:keycode event) (glop:keysym event) (glop:text event)))

  (dispatch glop:button-press-event
    (on-mouse-down window (glop:button event)))

  (dispatch glop:button-release-event
    (on-mouse-up window (glop:button event)))

  (dispatch glop:mouse-motion-event
    (on-mouse-move window (glop:x event) (glop:y event)))

  (dispatch glop:resize-event
    (on-resize window (glop:width event) (glop:height event)))

  (dispatch glop:expose-event
    (on-expose window))

  (dispatch glop:visibility-obscured-event
    (on-obscured window))

  (dispatch glop:visibility-unobscured-event
    (on-unobscured window))

  (dispatch glop:focus-in-event
    (on-focus window))

  (dispatch glop:focus-out-event
    (on-blur window))

  (dispatch glop:close-event
    (on-close window)))

(defmethod init :before ((window glop-window))
  (setf cl-opengl-bindings:*gl-get-proc-address* #'glop:gl-get-proc-address))

(defmethod on-update :before ((window glop-window) dt)
  (declare (ignore dt))
  (when (openp window)
   (loop
      for event = (glop:next-event (glop-window window) :blocking nil)
      while (openp window)
      do (if event
             (dispatch-glop-event window event)
             (return)))))

#+nil(defun key-down-p (engine key)
  "Is KEY being held down?"
  (values (gethash key (keys-held-down engine))))

(defmethod on-mouse-move :before ((window glop-window) x y)
  (with-accessors ((wx mouse-x) (wy mouse-y))
      window
    (setf wx x
          wy y)))

;;;
;;; Compatibility window
;;;
(defclass window (glop-window)
  ((view :accessor view :initarg :view)))

(defmethod initialize-instance :after ((window window) &key)
  (unless (slot-boundp window 'view)
    (setf (view window)
          (make-instance 'view
                         :top-edge (height window)
                         :right-edge (width window)))))

(defmethod on-resize ((window window) width height)
  (update-view (view window) 0 0 width height)
  (set-view (view window)))

