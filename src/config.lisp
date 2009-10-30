;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of until-it-dies

;;;; config.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

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
