(defpackage #:uid-demo
  (:use :cl :sheeple :until-it-dies))
(in-package :uid-demo)

(defsheep =test-engine= (uid::=engine=)
  ((title "Test Engine")
   (screens (list (clone (uid::=screen=) ())))
   (window-width 600)
   (window-height 600)))

(defparameter *test-image*
  (uid::create-image
   "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png"
   :x 50
   :y 50))

(defparameter *speed* 200)

(defmessage uid::update ((engine =test-engine=) dt)
  (declare (ignore engine))
  (when (uid::key-down-p :right)
    (incf (uid::x *test-image*) (* *speed* dt)))
  (when (uid::key-down-p :left)
    (decf (uid::x *test-image*) (* *speed* dt)))
  (when (uid::key-down-p :up)
    (incf (uid::y *test-image*) (* *speed* dt)))
  (when (uid::key-down-p :down)
    (decf (uid::y *test-image*) (* *speed* dt))))

(defmessage uid::draw ((engine =test-engine=))
  (draw :rectangle 100 100))
;; (uid::attach *test-image* =test-engine=)

;; VS

;; (defmessage uid::draw ((engine =test-engine=))
;;   (declare (ignore engine))
;;   (uid::draw *test-image*))

;; (defmessage uid::init ((engine =test-engine=))
;;   (declare (ignore engine))
;;   (uid::init *test-image*)
;;   (call-next-message))

(defbuzzword draw (foo x y))
(defmessage draw ((obj :rectangle) x y)
  (uid::draw-rectangle x y 20 20))
