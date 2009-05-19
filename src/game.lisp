;; This file is part of Until It Dies

;; game.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Game Prototype
;;;
(defsheep =game= ()
  ((event-queue (clone (=event-queue=) ())
		:cloneform (clone (=event-queue=) ()))
   (player nil)
   (background nil) 
   (projectiles nil :cloneform nil)
   (enemies nil :cloneform nil)))

(defbuzzword load-game (engine filename))
(defmessage load-game ((engine =engine=) filename)
  "Loads a game script and returns the configured GAME object."
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
  (declare (ignore game delta-t))
  "Takes care of calling UPDATE on all of GAME's member objects. Also, resolves collisions"
)

(defmessage draw ((game =game=))
  (with-properties (background player projectiles enemies messages) game
    (draw background)
    (mapc #'draw messages)
    (draw player)
    (mapc #'draw enemies)
    (mapc #'draw projectiles)))

(defmessage resolve-collisions ((game =game=))
  (declare (ignore game))
  nil)
