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
   (default-font (object :parents =font=))
   (clear-color (make-color :r 0 :g 0 :b 0 :a 0))
   pausedp
   (mouse-x 0)
   (mouse-y 0)
   (window-width 400)
   (window-height 400)
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
  (setf (keys-held-down engine) (make-hash-table)))

(defreply (setf title) :after (new-value (engine =engine=))
  (when (initializedp engine)
    (glfw:set-window-title new-value)))

(defreply (setf window-width) :after (new-value (engine =engine=))
  (declare (ignore new-value))
  (when (initializedp engine)
    (with-properties (window-width window-height) engine
      (glfw:set-window-size window-width window-height))))

(defreply (setf window-height) :after (new-value (engine =engine=))
  (declare (ignore new-value))
  (when (initializedp engine)
    (with-properties (window-width window-height) engine
      (glfw:set-window-size window-width window-height))))

(defreply update ((engine =engine=) dt &key)
  (declare (ignore dt))
  (values))

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
  (glfw:close-window))

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

(defreply idle ((engine =engine=))
  (let ((color (clear-color engine)))
    (with-properties (r g b a) color
      (gl:clear-color r g b a)))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (update-time engine)
  (process-cooked-events engine)
  (update engine (dt engine))
  (draw engine)
  (glfw:swap-buffers))

;;; Main loop
(defreply init :before ((engine =engine=))
  (setf (last-frame-time engine) 0)
  (setf cl-opengl-bindings:*gl-get-proc-address* #'cl-glfw:get-proc-address)
  (setup-ortho-projection (window-width engine) (window-height engine))
  (il:init)
  (ilut:init)
  (alut:init)
  (setf (initializedp engine) t))

(defreply init ((engine =engine=))
  "Do nothing by default."
  (values))

(defreply teardown ((engine =engine=))
  "Do nothing by default."
  (values))
(defreply teardown :after ((engine =engine=))
  "Once the real teardown stuff is done, we shut down our libs."
  (il:shutdown)
  (alut:exit)
  (glfw:terminate)
  (setf (initializedp engine) nil))

(defmacro with-engine (engine &body body)
  "This convenience macro simple makes sure the engine is torn down once
we're done with it."
  (let ((engine-var (gensym "ENGINE-")))
    `(let* ((,engine-var ,engine)
            (*engine* ,engine-var))
       (with-event-queue (event-queue ,engine-var)
         (with-resource-manager (resource-manager ,engine-var)
           (with-font (default-font ,engine-var)
             (init ,engine-var)
             (unwind-protect
                  ,@body
               (teardown ,engine-var))))))))

(cffi:defcallback key-hook :void ((key :int) (action :int))
  (case action
    (#.glfw:+press+
     (restartable (key-down *engine* (translate-key key))))
    (#.glfw:+release+
     (restartable (key-up *engine* (translate-key key))))))

(cffi:defcallback char-hook :void ((key :int) (action :int))
  (case action
    (#.glfw:+press+
     (restartable (key-down *engine* (translate-key key))))
    (#.glfw:+release+
     (restartable (key-up *engine* (translate-key key))))))

(cffi:defcallback mouse-moved :void ((x :int) (y :int))
  (restartable (mouse-move *engine* x (- (window-height *engine*) y))))

(cffi:defcallback mouse-button-hook :void ((button :int) (action :int))
  (case action
    (#.glfw:+press+
     (restartable (mouse-down *engine* (translate-key button))))
    (#.glfw:+release+
     (restartable (mouse-up *engine* (translate-key button))))))

(cffi:defcallback window-resized :void ((width :int) (height :int))
  (restartable (window-resized *engine* width height)))

(cffi:defcallback window-closed :void ()
  (setf (runningp *engine*) nil))

(defreply run ((engine =engine=))
  (glfw:with-init
    (glfw:open-window-hint glfw:+window-no-resize+ glfw:+true+)
    (glfw:with-open-window ((title engine) (window-width engine) (window-height engine))
      (with-engine engine
        (glfw:set-key-callback (cffi:callback key-hook))
        (glfw:set-char-callback (cffi:callback char-hook))
        (glfw:set-mouse-pos-callback (cffi:callback mouse-moved))
        (glfw:set-mouse-button-callback (cffi:callback mouse-button-hook))
        (glfw:set-window-size-callback (cffi:callback window-resized))
        (glfw:set-window-close-callback (cffi:callback window-closed))
        (setf (runningp engine) t)
        (loop while (= glfw:+true+ (glfw:get-window-param glfw:+opened+))
           do (restartable (idle engine))))))
  engine)
