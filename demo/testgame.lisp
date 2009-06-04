(in-package :until-it-dies)

(defsheep =test-engine= (=engine=)
  ((title "Test Engine")
   (screens (list (clone (=screen=) ())))))

(defun create-sprite (filepath &key (x 0) (y 0))
  (let* ((texture (create-texture filepath)))
    (clone (=sprite=)
	   ((x x)
	    (y y)
	    (texture texture)))))

(defparameter *test-sprite*
  (create-sprite
   "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png"))
;; UID> (attach (create-sprite "/path/to/texture.png")
;;              =test-engine=)
;;
;; UID> (run =test-engine=)
