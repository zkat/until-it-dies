;; This file is part of uid

;; game.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defsheep =game= ()
  ((running-p t)
   (keys-held-down (make-hash-table :test #'eq)
		   :cloneform (make-hash-table :test #'eq))
   (current-level nil)
   (game-over-p nil)
   (paused-p  nil)
   (screen-width *screen-width*)
   (screen-height *screen-height*)
   (title "(Shoot it) Until it Dies")))

;;;
;;; Buzzwords
;;;
(defbuzzword init (game)
  (:documentation "This buzzword takes care of any initialization that needs to be 
done before entering the game loop."))
(defbuzzword run (game)
  (:documentation "Runs the game. GAME is initialized inside this buzzword,
 followed by the main game loop."))
(defbuzzword update (object delta-t)
  (:documentation "Updates the state of the object by DELTA-T (which is in milliseconds)"))
(defbuzzword draw (object)
  (:documentation "Renders the object onto the screen in its current state."))

;; Events
(defbuzzword key-up (game key mod-key unicode state scancode)
  (:documentation "Key event for a key being released."))
(defbuzzword key-down (game key mod-key unicode state scancode)
  (:documentation "Key event for a key being pressed."))
(defbuzzword window-resized (game width height)
  (:documentation "Event called whenever the window is resized by the user"))
(defbuzzword mouse-up (game button state x y)
  (:documentation "A mouse button has been released."))
(defbuzzword mouse-down (game button state x y)
  (:documentation "A mouse button has been pressed."))
(defbuzzword mouse-move (game x y delta-x delta-y)
  (:documentation "Mouse has been moved to (X,Y)."))


;;; Game messages
(defmessage update ((game =game=) delta-t)
  "At the highest level, we simply forward the update message to the active level."
  #+nil(update (current-level game) delta-t)
  (update (sprite game) delta-t))

(defmessage draw :before ((game =game=))
	    (declare (ignore game))
	    (gl:clear :color-buffer-bit :depth-buffer-bit)
	    (gl:enable :texture-2d :blend)
	    (gl:blend-func :src-alpha :one-minus-src-alpha))

(defmessage draw ((game =game=))
  "We need to do some setup here, and call SDL:UPDATE-DISPLAY once everything else is rendered."
  (declare (ignore game))
  #+nil(draw (current-level game)))

(defmessage draw :after ((game =game=))
	    (declare (ignore game))
	    (sdl:update-display))

(defmessage window-resized (game width height)
  (declare (ignore game width height)))

;;; Key event handling
(defmessage key-up ((game =game=) key mod-key unicode state scancode)
  (declare (ignore mod-key unicode state scancode))
  (when (sdl:key= key :sdl-key-p)
    (toggle-pause))
  (with-properties (keys-held-down) game
    (setf (gethash key keys-held-down) nil)))

(defmessage key-down ((game =game=) key mod-key unicode state scancode)
  (declare (ignore mod-key unicode state scancode))
  (with-properties (keys-held-down) game
    (setf (gethash key keys-held-down) t)))

(defun key-down-p (key game)
  (with-properties (keys-held-down) game
    (let ((down-p (gethash key keys-held-down)))
      down-p)))

;;; Mouse event handling
;;; Only stubs here for now
(defmessage mouse-up ((game =game=) button state x y)
  (declare (ignore game button state x y))
  (values))
(defmessage mouse-down ((game =game=) button state x y)
  (declare (ignore game button state x y))
  (values))
(defmessage mouse-move ((game =game=) x y dx dy)
  (declare (ignore game x y dx dy))
  (values))


;;; Main loop
(defvar *game*)
(defmessage init ((game =game=))
  (sdl:window (screen-width game) (screen-height game)
	      :title-caption (title game)
	      :flags (logior sdl:sdl-opengl))
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
  (setup-ortho-projection (screen-width game) (screen-height game))
#+nil(load-level game "test-level"))

(defmessage run ((game =game=))
  (sdl:with-init ()
    (init game)
    (let ((last-time (now)))
      (sdl:with-events ()
	(:quit-event () (prog1 t
			  (setf (running-p game) nil)))
	(:video-resize-event (:w width :h height)
			     (window-resized game width height))
	(:key-down-event (:key key :mod-key mod-key :unicode unicode :state state :scancode scancode)
			 (restartable (key-down game key mod-key unicode state scancode)))
	(:key-up-event (:key key :mod-key mod-key :unicode unicode :state state :scancode scancode)
		       (restartable (key-up game key mod-key unicode state scancode)))
	(:mouse-button-up-event (:button button :state state :x x :y y)
				(restartable (mouse-up game button state x y)))
	(:mouse-button-down-event (:button button :state state :x x :y y)
				  (restartable (mouse-down game button state x y)))
	(:mouse-motion-event (:x x :y y :x-rel delta-x :y-rel delta-y)
			     (restartable (mouse-move game x y delta-x delta-y)))
	(:idle ()
	       (let* ((now (now))
		      (delta-t (- now last-time)))
		 (setf last-time now)
		 (restartable (update game delta-t)))
	       (restartable (draw game))))
      ;; We return the game after everything's done.
      ;; It might be handy for inspection.
      game
      )))
