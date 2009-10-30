;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; resources-late.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;; Anytime we change a font's dimensions while *engine* is initialized, we should reload it.
(defreply (setf size) :after (new-size (font =font=))
  (declare (ignore new-size))
  (when (initializedp *engine*)
    (load-resource font)))

(defreply (setf res) :after (new-res (font =font=))
  (declare (ignore new-res))
  (when (initializedp *engine*)
    (load-resource font)))
