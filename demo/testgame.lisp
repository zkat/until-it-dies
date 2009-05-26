(in-package :until-it-dies)

(defun create-texture (filepath)
  (clone (=file-texture=)
	 ((filepath filepath))))

(defsheep =test-engine= (=engine=)
  ((title "Test Engine")
   (screens (list (clone (=screen=) ())))))

(defsheep =test-sprite= (=sprite=)
  ((x 100)
   (y 100)
   (width 100)
   (height 100)
   (texture (create-texture
	     "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png"))))

(defun create-sprite (filepath &key (x 0) (y 0))
  (let ((texture (create-texture filepath)))
    (clone (=sprite=)
	   ((x x)
	    (y y)
	    (texture texture)
	    (width (width texture))
	    (height (height texture))))))

;; UID> (attach =test-sprite= =test-engine=)
;;
;; UID> (run =test-engine=)