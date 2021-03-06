;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; fonts.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Fonts
;;;
(defclass base-font ()
  ((size :initarg :size :accessor size)
   (resolution :initarg :resolution :accessor resolution :initform 100)))

(defvar *font*)

(defmacro with-font (font &body body)
  "Binds *default-font* to FONT within BODY."
  `(let ((*font* ,font))
     ,@body))

(defclass file-font (base-font file-resource)
  ())

(defmethod load-resource :before ((font file-font))
  (when (font-pointer font)
    (unload-resource font)))

;;;
;;; FTGL fonts
;;;
(defclass ftgl-font (file-font)
  ((font-pointer :initarg :font-pointer :accessor font-pointer :initform nil)
   (loadedp :accessor loadedp :initform nil)))

(defmethod load-resource ((font ftgl-font))
  (setf (font-pointer font)
        (uid-ftgl:create-texture-font (namestring (filepath font))))
  (uid-ftgl:set-font-face-size (font-pointer font)
                           (size font)
                           (resolution font))
  (setf (loadedp font) t)
  font)

(defmethod unload-resource ((font ftgl-font))
  (uid-ftgl:destroy-font (font-pointer font))
  (setf (font-pointer font) nil)
  (setf (loadedp font) nil)
  font)

(defmethod load-resource :after ((font ftgl-font))
  (let ((ptr (font-pointer font)))
    ;; TODO - I should have a more extensible finalization system...
    (finalize font (lambda ()
                     (uid-ftgl:destroy-font ptr)))))

;;;
;;; ZPB-TTF fonts
;;;
(defclass zpb-ttf-font (file-font)
  ((font-pointer :initarg :font-pointer :accessor font-pointer :initform nil)
   (loadedp :accessor loadedp :initform nil))
  (:documentation "A font is used by the text-drawing system to draw strings to screen."))

(defmethod load-resource ((font zpb-ttf-font))
  (setf (font-pointer font)
        (zpb-ttf:open-font-loader (namestring (filepath font))))
  (setf (loadedp font) t)
  font)

(defmethod load-resource :after ((font zpb-ttf-font))
  (let ((ptr (font-pointer font)))
    ;; TODO - I should have a more extensible finalization system...
    (finalize font (lambda ()
                     (zpb-ttf:close-font-loader ptr)))))

(defmethod unload-resource ((font zpb-ttf-font))
  (zpb-ttf:close-font-loader (font-pointer font))
  (setf (font-pointer font) nil)
  (setf (loadedp font) nil)
  font)

(defmethod (setf size) :after (new-size (font zpb-ttf-font))
  (declare (ignore new-size))
  (load-resource font))

(defmethod (setf resolution) :after (new-res (font zpb-ttf-font))
  (declare (ignore new-res))
  (load-resource font))
