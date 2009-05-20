;; This file is part of Until It Dies

;; game.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Game Prototype
;;;
(defsheep =screen= ()
  ((name 'default-screen)
   (initialized-p nil)
   (event-queue (clone (=event-queue=) ())
		:cloneform (clone (=event-queue=) ()))
   (input-enabled-p t)
   (components nil :cloneform nil)))

(defbuzzword load-screen (engine filename))
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
  ;; texture initialization goes here
  )
(defmessage teardown ((screen =screen=))
  (declare (ignore screen))
  ;; texture deletion goes here
  )
(defmessage update ((screen =screen=) delta-t)
  "Takes care of calling UPDATE on all of SCREEN's member objects. Also, resolves collisions"
  (mapc (lambda (obj)
	  (update obj delta-t))
	(components screen)))

(defmessage draw ((screen =screen=))
  (mapc #'draw (components screen)))

(defmessage attach ((screen =screen=) (engine =engine=))
  (push screen (screens engine)))

(defmessage detach ((screen =screen=) (engine =engine=))
  (setf (screens engine)
	(delete screen (screens engine))))
