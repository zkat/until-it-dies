(in-package :until-it-dies)

(defsheep =particle-system= ()
  ())

(defmessage draw ((system =particle-system=))
  (values))
(defmessage update ((system =particle-system=) delta-t)
  nil)

(defsheep =particle-manager= ()
  ((systems nil :cloneform nil)))

(defmessage draw ((manager =particle-manager=))
  (mapc #'draw (systems manager)))
(defmessage update ((manager =particle-manager=) delta-t)
  (setf (systems manager)
	(delete-if-not (lambda (x)
			 (update x delta-t))
		       (systems manager))))
