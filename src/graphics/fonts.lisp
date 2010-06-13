;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; fonts.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Fonts
;;;
(defclass font (file-resource)
  ((font-pointer :initarg :font-pointer :accessor font-pointer :initform nil)
   (size :initarg :size :accessor size)
   (res :initarg :res :accessor res :initform 100)
   (loadedp :accessor loadedp :initform nil))
  (:documentation "A font is used by the text-drawing system to draw strings to screen."))

(defvar *font*)

(defmacro with-font (font &body body)
  "Binds *default-font* to FONT within BODY."
  `(let ((*font* ,font))
     ,@body))

(defmethod load-resource :before ((font font))
  (when (font-pointer font)
    (unload-resource font)))

(defmethod load-resource ((font font))
  (setf (font-pointer font)
        (zpb-ttf:open-font-loader (namestring (filepath font))))
  (setf (loadedp font) t)
  font)

(defmethod load-resource :after ((font font))
  (let ((ptr (font-pointer font)))
    (finalize font (lambda ()
                     (zpb-ttf:close-font-loader ptr)))))

(defmethod unload-resource ((font font))
  (zpb-ttf:close-font-loader (font-pointer font))
  (setf (font-pointer font) nil)
  (setf (loadedp font) nil)
  font)

(defmethod (setf size) :after (new-size (font font))
  (declare (ignore new-size))
  (load-resource font))

(defmethod (setf res) :after (new-res (font font))
  (declare (ignore new-res))
  (load-resource font))

