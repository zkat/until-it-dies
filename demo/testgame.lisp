(in-package :until-it-dies)

(defsheep =test-engine= (=engine=)
  ((title "Test Engine")
   (screens (list (clone (=screen=) ())))))

(defparameter *test-image*
  (create-image
   "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png"
   :x 100
   :y 100))

(defmessage idle ((engine =test-engine=))
  (declare (ignore engine))
  (draw *test-image*))

(defmessage init ((engine =test-engine=))
  (init *test-image*)
  (call-next-message))