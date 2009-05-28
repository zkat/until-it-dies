;; This file is part of Until It Dies

;; screen.lisp
;;
;; * TODO - should collision detection go here?... (I think so...)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Screen Prototype
;;;
;;; - Screens are objects that contain components. They are rendered in sequence by the current
;;;   engine object, but are otherwise decoupled from each other.
;;;   Each screen also manages its own event-queue, which only fires its events if the screen
;;;   is currently attached to a screen (thus receiving the UPDATE message)
;;;   On update, =screen= objects take care of firing any cooked events in their event queue,
;;;   checking for collisions, and passing on the UPDATE and DRAW messages to any 
;;;   attached components. 
;;;   Screens are meant to be containers for entire blocks of app execution, and can
;;;   be loaded from file scripts (ideally without affecting the rest of the world).
(defsheep =screen= ()
  ((name '=screen=)
   (initialized-p nil)
   (event-queue (clone (=event-queue=) ())
		:cloneform (clone (=event-queue=) ()))
   (input-enabled-p t)
   (components nil :cloneform nil)))

(defbuzzword load-screen (engine filename))
;; TODO - get this to work again
(defmessage load-screen ((engine =engine=) filename)
  "Loads a screen definition file and returns the configured SCREEN object."
  (declare (ignore engine filename))
  #+nil(let ((*engine* (clone (=engine=)
		       ((current-screen (clone (=screen=) ()))))))
    (load (merge-pathnames (concatenate 'string name ".lisp") (get-screen-path)))
    (setf (background (current-screen *engine*)) (clone (=background=) ()))
    (setf (player (current-screen *engine*)) (clone (=player=) ()))
    (current-screen *engine*)))

;;;
;;; Buzzwords
;;;
(defbuzzword resolve-collisions (screen))

;;; Messages
(defmessage init ((screen =screen=))
  (declare (ignore screen))
  (values))

(defmessage teardown ((screen =screen=))
  (declare (ignore screen))
  (values))

(defmessage update ((screen =screen=) dt)
  "Takes care of calling UPDATE on all of SCREEN's member objects. Also, resolves collisions"
  (mapc (lambda (obj)
	  (update obj dt))
	(components screen)))

(defmessage draw ((screen =screen=))
  (mapc #'draw (components screen)))

(defmessage attach ((screen =screen=) (engine =engine=))
  (pushnew screen (screens engine)))

(defmessage detach ((screen =screen=) (engine =engine=))
  (setf (screens engine)
	(delete screen (screens engine))))

(defmessage detach-all ((screen =screen=))
  (setf (components screen) nil))