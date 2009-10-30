;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; messages.lisp
;;;;
;;;; Listing of all UID Messages
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defvar *engine*)

;;;
;;; Event processing messages
;;;
(defmessage execute-event (event)
  (:documentation
   "Takes care of executing a particular event."))

(defmessage cookedp (event)
  (:documentation
   "Is the event ready to fire?"))

(defmessage push-event (event queue)
  (:documentation "Adds EVENT to QUEUE"))

(defmessage peek-next-event (queue)
  (:documentation "Peeks at the next event in QUEUE without removing it.
                   Returns NIL if there are no queued events."))

(defmessage pop-next-event (queue)
  (:documentation "Returns the next available event, and removes it from QUEUE.
                   Returns NIL if there is nothing in the queue."))

(defmessage event-available-p (queue)
  (:documentation "Is there a cooked event available?"))

(defmessage clear-events (queue)
  (:documentation "Clears all events off the event queue"))

(defmessage process-next-event (queue)
  (:documentation "Grabs the next event from QUEUE and executes it."))

(defmessage process-cooked-events (queue)
  (:documentation "Processes all cooked events in QUEUE"))

;;;
;;; Resource loading messages
;;;
(defmessage load-resource (resource)
  (:documentation
   "Loads the resource's data into memory, activating it."))
(defmessage unload-resource (resource)
  (:documentation
"Unloads the resource's data from memory,
handling any freeing that needs to happen"))
(defmessage loadedp (resource)
  (:documentation
   "Is the resource currently correctly loaded and available for use?
It's worth pointing out that a simple loadedp flag on the object may
not be enough for all possible resource types. It's allowed, and even
advisable, that this buzzword check other things to make sure it is
correctly loaded (such as confirming that the texture ID is valid, in
the case of =texture= objects.)"))

;;;
;;; Resource management messages
;;;
(defmessage attach (resource manager)
  (:documentation "Sets up RESOURCE to be managed by MANAGER."))
(defmessage detach (resource manager)
  (:documentation "Ends the management of RESOURCE by MANAGER."))
(defmessage detach-all (manager)
  (:documentation "Removes all resources from MANAGER."))

;;;
;;; Texture messages
;;;
(defmessage bind-texture (texture))

(defmessage unbind-texture (texture))

(defmessage calculate-tex-coords (obj))

;;;
;;; Engine messages
;;;
(defmessage init (object)
  (:documentation
   "This buzzword takes care of any initialization that needs to be
done before entering the engine loop."))

(defmessage teardown (object)
  (:documentation
   "Takes care of all the teardown necessary to clean up ENGINE."))

(defmessage run (engine)
  (:documentation
   "Runs the engine. ENGINE is initialized inside this buzzword,
followed by the main engine loop."))

(defmessage update (object dt &key)
  (:documentation
   "Updates the state of the object by DT (in seconds)"))

(defmessage draw (object &key)
  (:documentation
   "Renders the object onto the screen in its current state."))

;;;
;;; Event messages
;;;
(defmessage key-up (engine key mod-keys)
  (:documentation "Key event for a key being released."))

(defmessage key-down (engine key mod-keys)
  (:documentation "Key event for a key being pressed."))

(defmessage mouse-up (engine button x y)
  (:documentation "A mouse button has been released."))

(defmessage mouse-down (engine button x y)
  (:documentation "A mouse button has been pressed."))

(defmessage mouse-move (engine x y)
  (:documentation "Mouse has been moved to (X,Y)."))

(defmessage window-resized (engine width height)
  (:documentation "The window is resized by the user"))

(defmessage idle (engine)
  (:documentation "Run once per game loop."))
