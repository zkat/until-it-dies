;; This file is part of until-it-dies

;; config.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

(defparameter *window-height* 400)
(defparameter *window-width* 400)
(defparameter *bg-color* sdl:*black*)
(defparameter *default-framerate* 60)
(defparameter *resource-path-name* "resources/")
(defparameter *level-path-name* "levels/")
(defun get-resource-path ()
 (merge-pathnames *resource-path-name*
		  #+ccl(concatenate 'string (ccl::current-directory-name) "/")
		  #+sbcl(values *default-pathname-defaults*)
		  #+clisp(ext:default-directory)))
(defun get-level-path ()
  (merge-pathnames *level-path-name*
		   #+clisp(ext:default-directory)
		   #+sbcl(values *default-pathname-defaults*)
		   #+ccl(concatenate 'string (ccl::current-directory-name) "/")))