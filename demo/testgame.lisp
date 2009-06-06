(in-package :until-it-dies)

(defsheep =test-engine= (=engine=)
  ((title "Test Engine")
   (screens (list (clone (=screen=) ())))))

(defparameter *test-image*
  (create-image
   "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png"
   :x 100
   :y 100))

(defmessage init ((engine =test-engine=))
  (declare (ignore engine))
  (call-next-message))

(defmessage draw ((engine =test-engine=))
  (declare (ignore engine))
  (dotimes (i 10000)
    (draw-point (make-point :x (random 400)
                            :y (random 400))
                :color *yellow*)))
