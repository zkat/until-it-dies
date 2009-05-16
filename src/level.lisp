;; This file is part of Until It Dies

;; level.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Level Prototype
;;;
(defsheep =level= ()
  ((event-queue (make-event-queue) :cloneform (make-event-queue))
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
    (unless (dead-p player)
      (update player))
    (mapc #'update enemies)
    (mapc #'update messages)))

(defmessage draw ((level =level=))
  (with-properties (background player projectiles enemies messages) level
    (draw background)
    (mapc #'draw messages)
    (unless (dead-p player)
      (draw player))
    (mapc #'draw enemies)
    (mapc #'draw projectiles)
#+nil(sdl:draw-string-shaded-* (format nil "Lives left: ~a" (lives (player level)))
			      5 5 sdl:*red* (sdl:color :a 0))
#+nil(sdl:draw-string-shaded-* (format nil "Enemies downed: ~a" (score (player level)))
			      5 15 sdl:*red* (sdl:color :a 0))
#+nil(sdl:draw-string-shaded-* (format nil "Current frame: ~a" (current-frame level))
			      5 25 sdl:*red* (sdl:color :a 0))))

(defmessage resolve-collisions ((level =level=))
  (with-properties (enemies projectiles player) level
    (loop for enemy in enemies
       do (loop for projectile in projectiles
	     do (cond ((and (eql (weapon player)
				 (shooter projectile))
			    (collided-p projectile enemy))
		       (decf (hp enemy))
		       (setf projectiles (delete projectile projectiles)))
		      ((collided-p projectile player)
		       (explode! player)
		       (explode! projectile)
		       (return-from resolve-collisions))
		      ((collided-p player enemy)
		       (explode! player)
		       (decf (hp enemy) 5)
		       (return-from resolve-collisions))
		      (t 
		       (values)))))))
