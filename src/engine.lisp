;; This file is part of Until It Dies

;; engine.lisp
;;
;; TODO - Wrap the sdl input stuff so that the key/modifier/mouseclick constants are
;;        UID symbols instead of stuff like :sdl-key-escape and such.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Engine Prototype
;;;
(defsheep =engine= ()
  ((runningp t)
   (initializedp nil)
   (last-frame-time 0)
   (dt 0)
   (keys-held-down (make-hash-table :test #'eq)
		   :cloneform (make-hash-table :test #'eq))
   (event-queue (clone (=event-queue=) ()))
   (resource-manager (clone (=resource-manager=) ()))
   (default-font (clone (=font=) ()))
   (clear-color (make-color :r 0 :g 0 :b 0 :a 0))
   (pausedp nil)
   (mouse-x 0)
   (mouse-y 0)
   (window-width 400 :writer nil)
   (window-height 400 :writer nil)
   (title "Until It Dies application")
   ;; fps calculation
   (fps 0)
   (fps-limit nil) ; only used during init time
   (fps-check-delay 2000)
   (last-fps-check-time 0)
   (last-fps-check-frame-count 0)
   (frame-count 0))
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

It's a good idea to clone =engine= for each application being created,
but it's not a mortal sin to just use it as a singleton."))

;;;
;;; Buzzwords
;;;
(defbuzzword init (object)
  (:documentation
"This buzzword takes care of any initialization that needs to be 
done before entering the engine loop."))

(defbuzzword teardown (object)
  (:documentation
"Takes care of all the teardown necessary to clean up ENGINE."))

(defbuzzword run (engine)
  (:documentation
"Runs the engine. ENGINE is initialized inside this buzzword,
followed by the main engine loop."))

(defbuzzword attach (a b)
  (:documentation
"Registers A with B. Used in cases such as attaching components to screens"))

(defbuzzword detach (a b)
  (:documentation
"Detaches A from B. Used in cases such as detaching components from screens"))

(defbuzzword detach-all (x)
  (:documentation
"Detaches everything from X."))

(defbuzzword update (object dt)
  (:documentation
"Updates the state of the object by DT (in seconds)"))

(defbuzzword draw (object)
  (:documentation
"Renders the object onto the screen in its current state."))

;; Events
(defbuzzword key-up (engine key mod-keys)
  (:documentation
"Key event for a key being released."))

(defbuzzword key-down (engine key mod-keys)
  (:documentation
"Key event for a key being pressed."))

(defbuzzword mouse-up (engine button x y)
  (:documentation
"A mouse button has been released."))

(defbuzzword mouse-down (engine button x y)
  (:documentation
"A mouse button has been pressed."))

(defbuzzword mouse-move (engine x y)
  (:documentation
"Mouse has been moved to (X,Y)."))

(defbuzzword window-resized (engine width height)
  (:documentation
"Event called whenever the window is resized by the user"))

(defbuzzword idle (engine)
  (:documentation
"Run once per game loop."))

;;;
;;; Engine messages
;;;
(defmessage update ((engine =engine=) dt)
  "At the highest screen, we simply forward the update message to the active screen."
  (declare (ignore engine dt))
  (values))

(defmessage draw ((engine =engine=))
  (declare (ignore engine))
  (values))

;;;
;;; Event handling
;;;

(defmessage process-cooked-events ((engine =engine=))
  (process-cooked-events (event-queue engine)))

;;; Key event handling

;;; TODO - should these also pass input events to all attached screens? Maybe not?
(defmessage key-up :before ((engine =engine=) key mod-keys)
  "If the key was released, it's no longer pressed!"
  (declare (ignore mod-keys))
  (with-properties (keys-held-down) engine
    (setf (gethash key keys-held-down) nil)))

(defmessage key-up ((engine =engine=) key mod-keys)
  "This is the 'real' KEY-UP. Blank by default."
  (declare (ignore engine key mod-keys))
  (values))

(defmessage key-down :before ((engine =engine=) key mod-keys)
  "If the key was pressed, then it's being held! :D"
  (declare (ignore mod-keys))
  (with-properties (keys-held-down) engine
    (setf (gethash key keys-held-down) t)))

(defmessage key-down ((engine =engine=) key mod-keys)
  "The 'real' key-down is blank by default."
  (declare (ignore engine key mod-keys))
  (values))

(defun key-down-p (key)
  "Is KEY being held down?"
  (with-properties (keys-held-down) *engine*
    (let ((down-p (gethash key keys-held-down)))
      down-p)))

;;; Mouse event handling

;;; - TODO: I should probably handle this, even with base =engine=. Keeping track of which mouse
;;;   buttons are held down, and the current x/y position of the cursor is probably a good plan.
(defmessage mouse-up ((engine =engine=) button x y)
  (declare (ignore engine button x y))
  (values))
(defmessage mouse-down ((engine =engine=) button x y)
  (declare (ignore engine x y))
  (when (eq button :))
  (values))
(defmessage mouse-move :before ((engine =engine=) x y)
  (with-properties (mouse-x mouse-y)
      engine
    (setf mouse-x x)
    (setf mouse-y y)))
(defmessage mouse-move ((engine =engine=) x y)
  (declare (ignore engine x y))
  (values))

;;; Other events
(defmessage window-resized (engine width height)
  "We don't really care about resize events right now."
  (declare (ignore engine width height))
  (values))

(defun update-time (engine)
  (with-properties (frame-count last-frame-time) engine
    (incf frame-count)
   (multiple-value-bind (dt now)
       (time-difference last-frame-time)
     (setf last-frame-time now)
     (setf (dt engine) dt)
     ;;    Update the current framerate for ENGINE
     (with-properties (last-fps-check-time last-fps-check-frame-count fps-check-delay fps frame-count)
	 engine
      (when (>= now (+ last-fps-check-time dt))
	(let ((frames (- frame-count last-fps-check-frame-count))
	      (seconds dt))
	  (setf fps (float (if (zerop dt)
			       0 (/ frames seconds))))
	  (setf last-fps-check-time now)
	  (setf last-fps-check-frame-count frame-count))))
     (process-cooked-events engine))))

(defmessage idle ((engine =engine=))
  (let ((color (clear-color engine)))
   (gl:clear-color (elt color 0)
                   (elt color 1)
                   (elt color 2)
                   (elt color 3)))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (update-time engine)
  (update engine (dt engine))
  (draw engine))

(defmessage idle :after ((engine =engine=))
  (declare (ignore engine))
  (sdl:update-display))

;;; Main loop
(defvar *engine* =engine=)
(defmessage init :before ((engine =engine=))
  "By default, we take care of setting sdl window options, 
and doing some very initial OpenGL setup."
  (sdl:window (window-width engine) (window-height engine)
	      :title-caption (title engine)
	      :flags (logior sdl:sdl-opengl))
  (setf (sdl:frame-rate) 0)
  (setf (frame-count engine) 0)
  (setf (last-frame-time engine) 0)
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
  (setup-ortho-projection (window-width engine) (window-height engine))
  (il:init)
  (ilut:init)
  (alut:init)
  (setf (initializedp engine) t))

(defmessage init ((engine =engine=))
  "Do nothing by default."
  (declare (ignore engine))
  (values))

(defmessage teardown ((engine =engine=))
  "Do nothing by default."
  (declare (ignore engine))
  (values))
(defmessage teardown :after ((engine =engine=))
  "Once the real teardown stuff is done, we shut down our libs."
  (il:shutdown)
  (alut:exit)
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

(defmessage run ((engine =engine=))
  "Here's the main loop -- because of the way lb-sdl is set up, 
we handle all input right here. We also bind the engine parameter
to *engine*, which might make things a little easier later on."
  (sdl:with-init ()
    (with-engine engine
     (sdl:with-events ()
       (:quit-event 
        () 
        (prog1 t
          (setf (runningp engine) nil)))
       (:video-resize-event
        (:w width :h height)
        (restartable (window-resized engine width height)))
       (:key-down-event
        (:key key :mod-key mod-keys)
        (restartable (key-down engine (translate-key key) (translate-key-list mod-keys))))
       (:key-up-event
        (:key key :mod-key mod-keys)
        (restartable (key-up engine (translate-key key) (translate-key-list mod-keys))))
       (:mouse-button-up-event
        (:button button :x x :y y)
        (restartable (mouse-up engine button x y)))
       (:mouse-button-down-event
        (:button button :x x :y y)
        (restartable (mouse-down engine button x y)))
       (:mouse-motion-event
        (:x x :y y)
        (restartable (mouse-move engine x (- (window-height engine) y))))
       (:idle
        ()
        (restartable (idle engine))))
     ;; We return the engine after everything's done.
     ;; It might be handy for inspection.
     engine)))
