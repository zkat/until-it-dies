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
   resizablep
   key-repeat-p
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

(defun create-engine (&key (default-font (object :parents =font=))
                      (clear-color *black*) resizablep (title "UID Application")
                      (window-width 500) (window-height 500) key-repeat-p)
  (defobject =engine= ((default-font default-font)
                       (clear-color clear-color)
                       (resizablep resizablep)
                       (title title) (key-repeat-p key-repeat-p)
                       (window-width window-width)
                       (window-height window-height))))

(defreply (setf key-repeat-p) :after (new-value (engine =engine=))
  (when (initializedp engine)
    (if new-value
        (uid-glfw:enable uid-glfw:+key-repeat+)
        (uid-glfw:disable uid-glfw:+key-repeat+))))

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

(defreply step ((engine =engine=))
  (let ((color (clear-color engine)))
    (with-properties (r g b a) color
      (gl:clear-color r g b a)))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (update-time engine)
  (process-cooked-events engine)
  (update engine (dt engine))
  (draw engine)
  (uid-glfw:swap-buffers))

;;; Main loop
(defreply init ((engine =engine=))
  (setf (last-frame-time engine) 0)
  (setf cl-opengl-bindings:*gl-get-proc-address* #'uid-glfw:get-proc-address)
  (setup-ortho-projection (window-width engine) (window-height engine))
  (uid-il:init)
  (uid-ilut:init)
  (alut:init)
  engine)

(defreply init :after ((engine =engine=))
  (when (key-repeat-p engine)
    (uid-glfw:enable uid-glfw:+key-repeat+))
  (setf (initializedp engine) t))

(defreply teardown ((engine =engine=))
  "Do nothing by default."
  (uid-il:shutdown)
  (alut:exit)
  (uid-glfw:terminate)
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
           (with-font (default-font ,engine-var)
             (init ,engine-var)
             (unwind-protect
                  ,@body
               (teardown ,engine-var))))))))

;;;
;;; Callbacks for GLFW events
;;;
(cffi:defcallback mouse-moved :void ((x :int) (y :int))
  (continuable (mouse-move *engine* x (- (window-height *engine*) y))))

(cffi:defcallback mouse-button-hook :void ((button :int) (action :int))
  (case action
    (#.uid-glfw:+press+
     (continuable (mouse-down *engine* button)))
    (#.uid-glfw:+release+
     (continuable (mouse-up *engine* button)))))

(cffi:defcallback window-resized :void ((width :int) (height :int))
  (continuable (window-resized *engine* width height)))

(cffi:defcallback window-closed :void ()
  (setf (runningp *engine*) nil))

;;;
;;; Main Function Extraordinaire
;;;
(defreply run ((engine =engine=))
  (uid-glfw:with-init
    (unless (resizablep engine)
      (uid-glfw:open-window-hint uid-glfw:+window-no-resize+ uid-glfw:+true+))
    (uid-glfw:with-open-window ((title engine) (window-width engine) (window-height engine))
      (with-engine engine
        (uid-glfw:set-key-callback (cffi:callback key-hook))
        (uid-glfw:set-char-callback (cffi:callback char-hook))
        (uid-glfw:set-mouse-pos-callback (cffi:callback mouse-moved))
        (uid-glfw:set-mouse-button-callback (cffi:callback mouse-button-hook))
        (uid-glfw:set-window-size-callback (cffi:callback window-resized))
        (uid-glfw:set-window-close-callback (cffi:callback window-closed))
        (setf (runningp engine) t)
        (loop while (= uid-glfw:+true+ (uid-glfw:get-window-param uid-glfw:+opened+))
           do (continuable (step engine))))))
  engine)
