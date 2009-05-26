(in-package :until-it-dies)

(defun create-texture (filepath)
  (clone (=file-texture=)
	 ((filepath filepath))))

(defsheep =test-engine= (=engine=)
  ((title "Test Engine")
   (screens (list (clone (=screen=) ())))))

(defun create-sprite (filepath &key (x 0) (y 0))
  (let* ((texture (load-resource (create-texture filepath)))
	 (width (width texture))
	 (height (height texture)))
    (clone (=sprite=)
	   ((x x)
	    (y y)
	    (texture texture)
	    (width width)
	    (height height)))))

;; UID> (attach =test-sprite= =test-engine=)
;;
;; UID> (run =test-engine=)