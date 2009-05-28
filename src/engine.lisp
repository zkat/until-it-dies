;; This file is part of Until It Dies

;; engine.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Engine Prototype
;;;
;;; - Engines are objects that contain information about -how- to run an application.
;;;   The engine handles the following aspects of UID applications:
;;;     * Framerate
;;;     * Input
;;;     * Global pausing
;;;     * Window height/width/title
;;;     * General initialization
;;;     * The main loop
;;;
;;;   In general, it's a good idea to clone =engine= for each application being created,
;;;   but it's not a mortal sin to just use it as a singleton.
(defsheep =engine= ()
  ((running-p t)
   (initialized-p nil)
   (fps 0)
   (fps-limit nil)
   (keys-held-down (make-hash-table :test #'eq)
		   :cloneform (make-hash-table :test #'eq))
   (screens nil)
   (paused-p nil)
   (window-width 400)
   (window-height 400)
   (title "Until it Dies")))

;;;
;;; Buzzwords
;;;
(defbuzzword init (object)
  (:documentation "This buzzword takes care of any initialization that needs to be 
done before entering the engine loop."))
(defbuzzword teardown (object)
  (:documentation "Takes care of all the teardown necessary to clean up ENGINE."))
(defbuzzword run (engine)
  (:documentation "Runs the engine. ENGINE is initialized inside this buzzword,
 followed by the main engine loop."))
(defbuzzword attach (a b)
  (:documentation "Registers A with B. Used in cases such as attaching components to screens"))
(defbuzzword detach (a b)
  (:documentation "Detaches A from B. Used in cases such as detaching components from screens"))
(defbuzzword detach-all (x)
  (:documentation "Detaches everything from X."))
(defbuzzword update (object dt)
  (:documentation "Updates the state of the object by DT (which is in milliseconds)"))
(defbuzzword draw (object)
  (:documentation "Renders the object onto the screen in its current state."))

;; Device Events
(defbuzzword key-up (engine key mod-key unicode state scancode)
  (:documentation "Key event for a key being released."))
(defbuzzword key-down (engine key mod-key unicode state scancode)
  (:documentation "Key event for a key being pressed."))
(defbuzzword key-down-p (engine key)
  (:documentation "Is KEY currently being held down?"))
(defbuzzword window-resized (engine width height)
  (:documentation "Event called whenever the window is resized by the user"))
(defbuzzword mouse-up (engine button state x y)
  (:documentation "A mouse button has been released."))
(defbuzzword mouse-down (engine button state x y)
  (:documentation "A mouse button has been pressed."))
(defbuzzword mouse-move (engine x y delta-x delta-y)
  (:documentation "Mouse has been moved to (X,Y)."))

;;;
;;; Engine messages
;;;

(defmessage update ((engine =engine=) dt)
  "At the highest screen, we simply forward the update message to the active screen."
  (mapc (lambda (screen)
	  (update screen dt))
	(screens engine)))

(defmessage draw :before ((engine =engine=))
	    "Clearing, and initial setup before drawing."
	    (declare (ignore engine))
	    (gl:clear-color 0 0 0 0)
	    (gl:clear :color-buffer-bit :depth-buffer-bit)
	    (gl:enable :texture-2d :blend)
	    (gl:blend-func :src-alpha :one-minus-src-alpha))

(defmessage draw ((engine =engine=))
  "The primary message will pass on the draw message to all of ENGINE's screens."
  (mapc #'draw (screens engine)))

(defmessage draw :after ((engine =engine=))
	    "Once everything is done, swap the back buffer in."
	    (declare (ignore engine))
	    (sdl:update-display))

(defmessage window-resized (engine width height)
  "We don't really care about resize events right now."
  (declare (ignore engine width height))
  (values))

;;; Key event handling
;;; 
;;; TODO - should these also pass input events to all attached screens? Maybe not?
(defmessage key-up :before ((engine =engine=) key mod-key unicode state scancode)
  "If the key was released, it's no longer pressed!"
  (declare (ignore mod-key unicode state scancode))
  (with-properties (keys-held-down) engine
    (setf (gethash key keys-held-down) nil)))

(defmessage key-up ((engine =engine=) key mod-key unicode state scancode)
  "This is the 'real' KEY-UP. Blank by default."
  (declare (ignore engine key mod-key unicode state scancode))
  (values))

(defmessage key-down :before ((engine =engine=) key mod-key unicode state scancode)
  "If the key was pressed, then it's being held! :D"
  (declare (ignore mod-key unicode state scancode))
  (with-properties (keys-held-down) engine
    (setf (gethash key keys-held-down) t)))

(defmessage key-down ((engine =engine=) key mod-key unicode state scancode)
  "The 'real' key-down is blank, although it keeps an eye on ESC, for Boss Protection™"
  (declare (ignore engine mod-key unicode state scancode))
  (when (sdl:key= key :sdl-key-escape)
    (sdl:push-quit-event)))

(defmessage key-down-p ((engine =engine=) key)
  "Is KEY being held down?"
  (with-properties (keys-held-down) engine
    (let ((down-p (gethash key keys-held-down)))
      down-p)))

;;; Mouse event handling
;;;
;;; - TODO: I should probably handle this, even with base =engine=. Keeping track of which mouse
;;;   buttons are held down, and the current x/y position of the cursor is probably a good
;;;   plan
(defmessage mouse-up ((engine =engine=) button state x y)
  (declare (ignore engine button state x y))
  (values))
(defmessage mouse-down ((engine =engine=) button state x y)
  (declare (ignore engine button state x y))
  (values))
(defmessage mouse-move ((engine =engine=) x y dx dy)
  (declare (ignore engine x y dx dy))
  (values))

;;; Attaching/detaching
(defmessage detach-all ((engine =engine=))
  "This simply removes all references to ENGINE's screens."
  (setf (screens engine) nil))

;;; Main loop
(defvar *engine*)
(defmessage init ((engine =engine=))
  "By default, we take care of setting sdl window options, 
and doing some very initial OpenGL setup."
  (sdl:window (window-width engine) (window-height engine)
	      :title-caption (title engine)
	      :flags (logior sdl:sdl-opengl))
  (setf (sdl:frame-rate) (or (fps-limit engine) 0))
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
  (setup-ortho-projection (window-width engine) (window-height engine))
  #+nil(push (screens engine)
	     (init (load-screen engine "menu"))))

(defmessage teardown ((engine =engine=))
  "Simply pass the message along to ENGINE's screens."
  (when (screens engine)
    (mapc #'teardown (screens engine))))

(defmessage run ((engine =engine=))
  "Here's the main loop -- because of the way lb-sdl is set up, we handle all input right here.
We also bind the engine parameter to *engine*, which might make things a little easier later on."
  (sdl:with-init ()
    (init engine)
    (let ((last-time (now))
	  (*engine* engine))
      (unwind-protect
	   ;; Better make sure we are always able to pass on
	   ;; the TEARDOWN message at the end of the loop.
	   (sdl:with-events ()
	     (:quit-event () (prog1 t
			       (setf (running-p engine) nil)))
	     (:video-resize-event (:w width :h height)
				  (window-resized engine width height))
	     (:key-down-event (:key key :mod-key mod-key :unicode unicode 
			       :state state :scancode scancode)
			      (restartable (key-down engine key mod-key unicode state scancode)))
	     (:key-up-event (:key key :mod-key mod-key :unicode unicode
                             :state state :scancode scancode)
			    (restartable (key-up engine key mod-key unicode state scancode)))
	     (:mouse-button-up-event (:button button :state state :x x :y y)
				     (restartable (mouse-up engine button state x y)))
	     (:mouse-button-down-event (:button button :state state :x x :y y)
				       (restartable (mouse-down engine button state x y)))
	     (:mouse-motion-event (:x x :y y :x-rel delta-x :y-rel delta-y)
				  (restartable (mouse-move engine x y delta-x delta-y)))
	     (:idle ()
		    (let* ((now (now))
			   (dt (- now last-time)))
		      (setf last-time now)
		      ;; Update the current framerate for ENGINE
		      (setf (fps engine) (/ 1000 (if (= 0 dt)
						     1 dt)))
		      ;; And pass on the UPDATE message.
		      (restartable (update engine dt)))
		    ;; Once everything's updated, draw the universe.
		    (restartable (draw engine))))
	;; Once out of the loop, we should tear everything down so resources get properly unloaded.
	(restartable (teardown engine)))
      ;; We return the engine after everything's done.
      ;; It might be handy for inspection.
      engine)))

