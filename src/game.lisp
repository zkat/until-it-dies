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
  (declare (ignore engine))
  (let ((*engine* (clone (=engine=)
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
  ;; texture initialization goes here
  )
(defmessage teardown ((game =game=))
  ;; texture deletion goes here
  )
(defmessage update ((game =game=))
  "Takes care of calling UPDATE on all of GAME's member objects. Also, resolves collisions"
  (with-properties (background player projectiles enemies messages current-frame) game
    (unless (dead-p player)
      (resolve-collisions game))
    (process-cooked-events game)
    (update background)
    (mapc #'update projectiles)
    (update player)
    (mapc #'update enemies)
    (mapc #'update messages)))

(defmessage draw ((game =game=))
  (with-properties (background player projectiles enemies messages) game
    (draw background)
    (mapc #'draw messages)
    (draw player)
    (mapc #'draw enemies)
    (mapc #'draw projectiles)))

(defmessage resolve-collisions ((game =game=))
  nil)
