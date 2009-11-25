;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; engine.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Engine Prototype
;;;
(defproto =engine= ()
  ((runningp t)
   initializedp
   (last-frame-time 0)
   (dt 0)
   (keys-held-down (make-hash-table))
   (event-queue (object :parents =event-queue=))
   (resource-manager (object :parents =resource-manager=))
   (clear-color (make-color :r 0 :g 0 :b 0 :a 0))
   pausedp
   resizablep
   (windowedp t)
   key-repeat-p
   (mouse-visible-p t)
   (mouse-x 0)
   (mouse-y 0)
   (last-mouse-wheel-position 0)
   joysticks
   (window-width 400)
   (window-height 400)
   (current-view (object :parents =view=))
   (title "Until It Dies application"))
  (:documentation
   "Engines are objects that contain information about
-how- to run an application.

The engine handles the following aspects of UID applications:
    * Framerate
    * Input
    * Global pausing
    * Global (default) event-queue
    * Window height/width/title
    * General initialization
    * The main loop

It's a good idea to create a delegate of =engine= for each application being created,
but it's not a mortal sin to just use it as a singleton."))

(defreply init-object :after ((engine =engine=) &key)
  (setf (keys-held-down engine) (make-hash-table)
        (resource-manager engine) (object :parents =resource-manager=)
        (event-queue engine) (object :parents =event-queue=)
        (current-view engine) (create-view 0 0 (window-width engine) (window-height engine))))

(defun create-engine (&key (clear-color *black*) resizablep (title "UID Application")
                      (window-width 500) (window-height 500) key-repeat-p
                      (mouse-visible-p t) (windowedp t) current-view)
  (defobject =engine= ((clear-color clear-color)
                       (resizablep resizablep)
                       (title title) (key-repeat-p key-repeat-p)
                       (windowedp windowedp)
                       (window-width window-width)
                       (window-height window-height)
                       (mouse-visible-p mouse-visible-p)
                       (current-view (or current-view (create-view 0 0 window-width window-height))))))

(defreply (setf key-repeat-p) :after (new-value (engine =engine=))
  (when (initializedp engine)
    (if new-value
        (uid-glfw:enable :key-repeat)
        (uid-glfw:disable :key-repeat))))

(defreply (setf mouse-visible-p) :after (new-value (engine =engine=))
  (when (initializedp engine)
    (if new-value
        (uid-glfw:enable :mouse-cursor)
        (uid-glfw:disable :mouse-cursor))))

(defreply (setf title) :after (new-value (engine =engine=))
  (when (initializedp engine)
    (uid-glfw:set-window-title new-value)))

(defreply (setf window-width) :after (new-value (engine =engine=))
  (declare (ignore new-value))
  (when (initializedp engine)
    (with-properties (window-width window-height) engine
      (uid-glfw:set-window-size window-width window-height))))

(defreply (setf window-height) :after (new-value (engine =engine=))
  (declare (ignore new-value))
  (when (initializedp engine)
    (with-properties (window-width window-height) engine
      (uid-glfw:set-window-size window-width window-height))))

(defreply update ((engine =engine=) dt &key)
  (declare (ignore dt))
  (values))

(defreply update :before ((engine =engine=) dt &key)
  (declare (ignore dt))
  (mapcar (fun (update-joystick engine _)) (joysticks engine)))

(defreply draw ((engine =engine=) &key)
  (values))

;;;
;;; Event handling
;;;
(defreply process-cooked-events ((engine =engine=))
  (process-cooked-events (event-queue engine)))

;;; Key event handling

;;; TODO - should these also pass input events to all attached screens? Maybe not?
(defreply key-up :before ((engine =engine=) key)
  "If the key was released, it's no longer pressed!"
  (with-properties (keys-held-down) engine
    (setf (gethash key keys-held-down) nil)))

(defreply key-up ((engine =engine=) key)
  "This is the 'real' KEY-UP. Blank by default."
  (declare (ignore key))
  (values))

(defreply key-down :before ((engine =engine=) key)
  "If the key was pressed, then it's being held! :D"
  (with-properties (keys-held-down) engine
    (setf (gethash key keys-held-down) t)))

(defreply key-down ((engine =engine=) key)
  "The 'real' key-down is blank by default."
  (when (eq key :escape)
    (quit))
  (values))

(defun quit ()
  (uid-glfw:close-window))

(defun key-down-p (key)
  "Is KEY being held down?"
  (with-properties (keys-held-down) *engine*
    (let ((down-p (gethash key keys-held-down)))
      down-p)))

;;; Mouse event handling

;;; - TODO: I should probably handle this, even with base =engine=. Keeping track of which mouse
;;;   buttons are held down, and the current x/y position of the cursor is probably a good plan.
(defreply mouse-up ((engine =engine=) button)
  (declare (ignore engine button))
  (values))
(defreply mouse-down ((engine =engine=) button)
  (declare (ignore engine button))
  (values))
(defreply mouse-move :before ((engine =engine=) x y)
  (with-properties (mouse-x mouse-y)
      engine
    (setf mouse-x x)
    (setf mouse-y y)))
(defreply mouse-move ((engine =engine=) x y)
  (declare (ignore engine x y))
  (values))

;;; joystick events
(defreply joystick-button-down ((engine =engine=) joystick button)
  (declare (ignore joystick button))
  (values))

(defreply joystick-button-up ((engine =engine=) joystick button)
  (declare (ignore joystick button))
  (values))

(defreply joystick-move ((engine =engine=) joystick axis state)
  (declare (ignore joystick axis state))
  (values))

(defun update-joystick (engine joystick)
  (with-properties (joystick-number num-axes num-buttons axis-positions button-states)
      joystick
    (let ((old-axis-positions axis-positions)
          (old-button-states button-states)
          (new-axis-positions (glfw-joystick-axis-positions joystick-number num-axes))
          (new-button-states (glfw-joystick-button-states joystick-number num-buttons)))
      (maybe-fire-joystick-axis-events engine joystick old-axis-positions new-axis-positions)
      (maybe-fire-joystick-button-events engine joystick old-button-states new-button-states)
      (setf axis-positions new-axis-positions
            button-states new-button-states)
      t)))

(defun maybe-fire-joystick-axis-events (engine joystick old-axes new-axes)
  (loop for old-axis in old-axes
     for new-axis in new-axes
     for axis-id from 0
     unless (= old-axis new-axis)
     do (joystick-move engine joystick axis-id new-axis)))

(defun maybe-fire-joystick-button-events (engine joystick old-buttons new-buttons)
  (loop for old-button in old-buttons
     for new-button in new-buttons
     for button-id from 0
     unless (eq old-button new-button)
     do (if (eq new-button :released)
            (joystick-button-up engine joystick button-id)
            (joystick-button-down engine joystick button-id))))

;;; Other events
(defreply window-resized (engine width height)
  "We don't really care about resize events right now."
  (setf (window-width engine) width)
  (setf (window-height engine) height)
  (values))

(defun update-time (engine)
  (with-properties (dt last-frame-time) engine
    (multiple-value-bind (new-dt now)
        (time-difference last-frame-time)
      (setf last-frame-time now)
      (setf dt new-dt))))

(defreply step-engine ((engine =engine=))
  (let ((color (clear-color engine)))
    (with-properties (red green blue alpha) color
      (gl:clear-color red green blue alpha)))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (update-time engine)
  (process-cooked-events engine)
  (update engine (dt engine))
  (draw engine)
  (uid-glfw:swap-buffers))

;;; Main loop
(defreply init ((engine =engine=))
  (setf (last-frame-time engine) 0)
  (setf (joysticks engine) (available-joysticks))
  (setf cl-opengl-bindings:*gl-get-proc-address* #'uid-glfw:get-proc-address)
  (set-view (current-view engine))
  engine)

(defreply init :after ((engine =engine=))
  (setf (mouse-visible-p engine) (mouse-visible-p engine)
        (key-repeat-p engine) (key-repeat-p engine))
  (setf (initializedp engine) t))

(defreply teardown ((engine =engine=))
  "Do nothing by default."
  engine)

(defreply teardown :after ((engine =engine=))
  "Once the real teardown stuff is done, we shut down our libs."
  (setf (initializedp engine) nil))

(defmacro with-engine (engine &body body)
  "This convenience macro simple makes sure the engine is torn down once
we're done with it."
  (let ((engine-var (gensym "ENGINE-")))
    `(let* ((,engine-var ,engine)
            (*engine* ,engine-var))
       (with-event-queue (event-queue ,engine-var)
         (with-resource-manager (resource-manager ,engine-var)
           (init ,engine-var)
           (unwind-protect
                ,@body
             (teardown ,engine-var)))))))

;;;
;;; Callbacks for GLFW events
;;;
(cffi:defcallback mouse-moved :void ((x :int) (y :int))
  (continuable
    (let* ((view (current-view *engine*))
           (width-factor (/ (view-width view) (window-width *engine*)))
           (height-factor (/ (view-height view) (window-height *engine*))))
      (mouse-move *engine* (+ (view-left view) (* x width-factor))
                  (+ (view-bottom view) (* (- (window-height *engine*) y) height-factor))))))

(cffi:defcallback mouse-button-hook :void ((button :int) (action uid-glfw:key/button-state))
  (case action
    (:press
     (continuable (mouse-down *engine* button)))
    (:release
     (continuable (mouse-up *engine* button)))))

(cffi:defcallback mouse-wheel-moved :void ((new-pos :int))
  (continuable (let ((delta (- new-pos (last-mouse-wheel-position *engine*))))
                 (if (> delta 0)
                     (progn (mouse-down *engine* 3)
                            (mouse-up *engine* 3))
                     (progn (mouse-down *engine* 4)
                            (mouse-up *engine* 4)))))
  (setf (last-mouse-wheel-position *engine*) new-pos))

(cffi:defcallback window-resized :void ((width :int) (height :int))
  (continuable (window-resized *engine* width height)))

(cffi:defcallback window-closed :void ()
  (setf (runningp *engine*) nil))

;;;
;;; Main Function Extraordinaire
;;;
(defreply run ((engine =engine=))
  (uid-glfw:with-init
    (uid-glfw:open-window-hint :window-no-resize (not (resizablep engine)))
    (if (uid-glfw:open-window (window-width engine) (window-height engine)
                              :mode (if (windowedp engine) :window :fullscreen))
        (with-engine engine
          (uid-glfw:set-window-title (title engine))
          (uid-glfw:set-key-callback (cffi:callback key-hook))
          (uid-glfw:set-char-callback (cffi:callback char-hook))
          (uid-glfw:set-mouse-pos-callback (cffi:callback mouse-moved))
          (uid-glfw:set-mouse-button-callback (cffi:callback mouse-button-hook))
          (uid-glfw:set-mouse-wheel-callback (cffi:callback mouse-wheel-moved))
          (uid-glfw:set-window-size-callback (cffi:callback window-resized))
          (uid-glfw:set-window-close-callback (cffi:callback window-closed))
          (setf (runningp engine) t)
          (loop while (uid-glfw:get-window-param :opened)
             do (continuable (step-engine engine))))
        (error "Opening the window failed.")))
  engine)
