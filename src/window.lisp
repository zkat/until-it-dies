;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; window.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Base Events
;;;
(defgeneric init (object)
  (:documentation
   "Initializes OBJECT."))

(defgeneric teardown (object)
  (:documentation
   "Wraps up any loose ends with OBJECT."))

(defgeneric on-update (object dt)
  (:documentation
   "Updates the state of OBJECT by DT (delta time, in seconds)"))

(defgeneric on-draw (object)
  (:documentation
   "Renders the object in its current state."))

;;;
;;; Windows
;;;
(defclass window ()
  ((title :initform "UID Window" :accessor title :initarg :title)
   (width :initform 800 :accessor width :initarg :width)
   (height :initform 600 :accessor height :initarg :height)
   (fullscreenp :initform nil :accessor fullscreenp :initarg :fullscreenp)
   (resizablep :initform nil :accessor resizablep :initarg :resizablep)
   (openp :initform nil :accessor openp)
   (key-repeat-p :initform nil :accessor key-repeat-p)
   (mouse-x :initform 0 :accessor mouse-x)
   (mouse-y :initform 0 :accessor mouse-y)
   (glop-window :initform nil :accessor glop-window)
   (view :accessor view :initarg :view)))

(defmethod initialize-instance :after ((window window) &key)
  (unless (slot-boundp window 'view)
    (setf (view window)
          (make-instance 'view
                         :top-edge (height window)
                         :right-edge (width window)))))

(defmethod (setf title) :after (new-value (window window))
  (glop:set-window-title (glop-window window) new-value))

(defmethod (setf fullscreenp) :after (new-value (window window))
  (glop:set-fullscreen (glop-window window) new-value))

;; TODO - not sure if glop supports this yet
(defgeneric (setf height) (new-value window))
(defgeneric (setf width) (new-value window))

(defgeneric set-gl-window (window)
  (:method ((window window))
    (glop:set-gl-window (glop-window window))
    window))

(defgeneric swap-buffers (window)
  (:method ((window window))
    (glop:swap-buffers (glop-window window))
    window))

(defgeneric open-window (window)
  (:method ((window window))
    (unless (openp window)
     (setf (glop-window window)
           (glop:create-window (title window)
                               (width window)
                               (height window)
                               :fullscreen (fullscreenp window)
                               :double-buffer t
                               :stencil-buffer t
                               :stencil-size 16
                               :accum-buffer t))
     (set-gl-window window)
     (set-view (view window))
     (setf (openp window) t))
    window))

(defgeneric close-window (window)
  (:method ((window window))
    (when (openp window)
     (glop:destroy-window (glop-window window))
     (setf (openp window) nil))
    window))

;;;
;;; Events
;;;
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
  (defev on-resize (window width height))
  (defev on-expose (window))
  (defev on-obscured (window))
  (defev on-unobscured (window))
  (defev on-focus (window))
  (defev on-blur (window))
  (defev on-close (window)))

(defgeneric dispatch-event (window event))
(macrolet ((dispatch (event-class-name &body body)
             `(defmethod dispatch-event ((window window) (event ,event-class-name))
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

(defmethod init ((window window))
  (open-window window)
  (values))

(defmethod teardown ((window window))
  (close-window window)
  (values))

(defmethod on-update :before ((window window) dt)
  (declare (ignore dt))
  (when (openp window)
   (loop
      while (openp window)
      for event = (glop:next-event (glop-window window) :blocking nil)
      while event
      do (dispatch-event window event))))

(defmethod on-update ((window window) dt)
  (declare (ignore dt))
  (values))

(defmethod on-draw ((window window))
  (values))

(defmethod on-close ((window window))
  (teardown window))

(defmethod on-resize ((window window) width height)
  (update-view (view window) 0 0 width height)
  (set-view (view window)))

#+nil(defun key-down-p (engine key)
  "Is KEY being held down?"
  (values (gethash key (keys-held-down engine))))

;;; Mouse event handling

(defmethod on-mouse-move :before ((window window) x y)
  (with-accessors ((wx mouse-x) (wy mouse-y))
      window
    (setf wx x
          wy y)))


;;; joystick events
;; (defreply joystick-button-down ((engine =engine=) joystick button)
;;   (declare (ignore joystick button))
;;   (values))

;; (defreply joystick-button-up ((engine =engine=) joystick button)
;;   (declare (ignore joystick button))
;;   (values))

;; (defreply joystick-move ((engine =engine=) joystick axis state)
;;   (declare (ignore joystick axis state))
;;   (values))

;; (defun update-joystick (engine joystick)
;;   (with-properties (joystick-number num-axes num-buttons axis-positions button-states)
;;       joystick
;;     (let ((old-axis-positions axis-positions)
;;           (old-button-states button-states)
;;           (new-axis-positions (glfw-joystick-axis-positions joystick-number num-axes))
;;           (new-button-states (glfw-joystick-button-states joystick-number num-buttons)))
;;       (maybe-fire-joystick-axis-events engine joystick old-axis-positions new-axis-positions)
;;       (maybe-fire-joystick-button-events engine joystick old-button-states new-button-states)
;;       (setf axis-positions new-axis-positions
;;             button-states new-button-states)
;;       t)))

;; (defun maybe-fire-joystick-axis-events (engine joystick old-axes new-axes)
;;   (loop for old-axis in old-axes
;;      for new-axis in new-axes
;;      for axis-id from 0
;;      unless (= old-axis new-axis)
;;      do (joystick-move engine joystick axis-id new-axis)))

;; (defun maybe-fire-joystick-button-events (engine joystick old-buttons new-buttons)
;;   (loop for old-button in old-buttons
;;      for new-button in new-buttons
;;      for button-id from 0
;;      unless (eq old-button new-button)
;;      do (if (eq new-button :released)
;;             (joystick-button-up engine joystick button-id)
;;             (joystick-button-down engine joystick button-id))))

;;; Other events
#+nil(defreply window-resized (engine width height)
  "We don't really care about resize events right now."
  (setf (window-width engine) width)
  (setf (window-height engine) height)
  (values))
