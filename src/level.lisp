;; This file is part of Until It Dies

;; level.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Level Prototype
;;;
(defsheep =level= ()
  ((event-queue (clone (=event-queue=) ())
		:cloneform (clone (=event-queue=) ()))
   (player nil)
   (background nil) 
   (projectiles nil :cloneform nil)
   (enemies nil :cloneform nil)))

(defbuzzword load-level (game filename))
(defmessage load-level ((game =game=) filename)
  "Loads a level script and returns the configured LEVEL object."
  (let ((*game* (clone (=game=)
		       ((current-level (clone (=level=) ()))))))
    (load (merge-pathnames (concatenate 'string name ".lisp") (get-level-path)))
    (setf (background (current-level *game*)) (clone (=background=) ()))
    (setf (player (current-level *game*)) (clone (=player=) ()))
    (current-level *game*)))

;;;
;;; Buzzwords
;;;
(defbuzzword resolve-collisions (level))

;;; Messages
(defmessage update ((level =level=))
  "Takes care of calling UPDATE on all of LEVEL's member objects. Also, resolves collisions"
  (with-properties (background player projectiles enemies messages current-frame) level
    (unless (dead-p player)
      (resolve-collisions level))
    (process-cooked-events level)
    (incf current-frame)
    (update background)
    (mapc #'update projectiles)
    (update player)
    (mapc #'update enemies)
    (mapc #'update messages)))

(defmessage draw ((level =level=))
  (with-properties (background player projectiles enemies messages) level
    (draw background)
    (mapc #'draw messages)
    (draw player)
    (mapc #'draw enemies)
    (mapc #'draw projectiles)))

(defmessage resolve-collisions ((level =level=))
  nil)
