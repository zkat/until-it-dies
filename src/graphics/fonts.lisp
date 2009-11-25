;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; fonts.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Fonts
;;;
(defproto =font= (=file-resource=)
  (font-pointer
   (size 10)
   (res 100)
   loadedp)
  :documentation "A font is used by the text-drawing system to draw strings to screen.")

(defreply create ((font =font=) &key filepath (size 12) (res 20))
  (unless filepath (error "Must provide a filepath."))
  (defobject (=font=) ((filepath filepath) (size size) (res res))))

(defun create-font (filepath &key (size 12) (res 20))
  (defobject (=font=) ((filepath filepath) (size size) (res res))))

(defvar *font* =font=)

(defmacro with-font (font &body body)
  "Binds *default-font* to FONT within BODY."
  `(let ((*font* ,font))
     ,@body))

(defreply load-resource :before ((font =font=))
  (when (font-pointer font)
    (unload-resource font)))

(defreply load-resource ((font =font=))
  (setf (font-pointer font)
        (zpb-ttf:open-font-loader (namestring (filepath font))))
  (setf (loadedp font) t)
  font)

(defreply load-resource :after ((font =font=))
  (let ((ptr (font-pointer font)))
    (finalize font (lambda ()
                     (zpb-ttf:close-font-loader ptr)))))

(defreply unload-resource ((font =font=))
  (zpb-ttf:close-font-loader (font-pointer font))
  (setf (font-pointer font) nil)
  (setf (loadedp font) nil)
  font)

;; Anytime we change a font's dimensions while *engine* is initialized, we should reload it.
(defreply (setf size) :after (new-size (font =font=))
  (declare (ignore new-size))
  (when (initializedp *engine*)
    (load-resource font)))

(defreply (setf res) :after (new-res (font =font=))
  (declare (ignore new-res))
  (when (initializedp *engine*)
    (load-resource font)))

