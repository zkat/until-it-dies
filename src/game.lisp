;; This file is part of Until It Dies

;; game.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Game Prototype
;;;
(defsheep =game= ()
  ((name 'default-uid-game)
   (initialized-p nil)
   (event-queue (clone (=event-queue=) ())
		:cloneform (clone (=event-queue=) ()))
   (input-enabled-p t)
   (components nil :cloneform nil)))

(defbuzzword load-game (engine filename))
(defmessage load-game ((engine =engine=) filename)
  "Loads a game definition file and returns the configured GAME object."
  (declare (ignore engine filename))
  #+nil(let ((*engine* (clone (=engine=)
		       ((current-game (clone (=game=) ()))))))
    (load (merge-pathnames (concatenate 'string name ".lisp") (get-game-path)))
    (setf (background (current-game *engine*)) (clone (=background=) ()))
    (setf (player (current-game *engine*)) (clone (=player=) ()))
    (current-game *engine*)))

;;;
;;; Buzzwords
;;;
(defbuzzword resolve-collisions (game))

;;; Messages
(defmessage init ((game =game=))
  (declare (ignore game))
  ;; texture initialization goes here
  )
(defmessage teardown ((game =game=))
  (declare (ignore game))
  ;; texture deletion goes here
  )
(defmessage update ((game =game=) delta-t)
  "Takes care of calling UPDATE on all of GAME's member objects. Also, resolves collisions"
  (mapc (lambda (obj)
	  (update obj delta-t))
	(components game)))

(defmessage draw ((game =game=))
  (mapc #'draw (components game)))
