(defpackage #:uid-demo
  (:use :cl :sheeple :until-it-dies))
(in-package :uid-demo)

(defsheep =test-engine= (uid::=engine=)
  ((title "Test Engine")
   (window-width 600)
   (window-height 600)))

(defparameter *test-image*
  (uid::create-image
   "/home/zkat/hackery/lisp/until-it-dies/res/lisplogo_alien_256.png"))

(defparameter *x* 50)
(defparameter *y* 50)
(defparameter *speed* 200)

(defmessage uid::update ((engine =test-engine=) dt)
  (declare (ignore engine))
  (when (uid::key-down-p :right)
    (incf *x* (* *speed* dt)))
  (when (uid::key-down-p :left)
    (decf *x* (* *speed* dt)))
  (when (uid::key-down-p :up)
    (incf *y* (* *speed* dt)))
  (when (uid::key-down-p :down)
    (decf *y* (* *speed* dt))))

(defmessage uid::draw ((engine =test-engine=))
  (declare (ignore engine))
  #+nil(gl:with-pushed-matrix
    (gl:translate 50 50 0)
    (ftgl:render-font (uid::font-pointer uid::*font*) "foobar" :all))
  (dotimes (i 500)
   (uid::draw-sprite "foo" 
                     (random 600)
                     (random 600)))
  (uid::draw-sprite *test-image* *x* *y*))
