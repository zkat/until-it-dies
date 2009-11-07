(defpackage uid-ftgl
  (:use :cl :cffi))
(in-package :uid-ftgl)

(define-foreign-library ftgl
  (:unix (:or "libftgl" "libftgl.so.2"))
  (t (:default "libftgl")))
(use-foreign-library ftgl)

(defcenum render-mode
  (:front 1)
  (:back 2)
  (:side 4)
  (:all #xffff))

(define-foreign-type pathname-string-type ()
  ()
  (:actual-type :string)
  (:simple-parser pathname-string))
(eval-when (:compile-toplevel :load-toplevel)
  (defmethod expand-to-foreign-dyn (value var body (type pathname-string-type))
    `(with-foreign-string (,var (if (pathnamep ,value) (namestring ,value) ,value))
       ,@body)))

(defctype font :pointer)
(defcfun ("ftglDestroyFont" destroy-font) :void "Destroy an FTGL font object." (font font))
(defcfun ("ftglSetFontFaceSize" set-font-face-size) :int "Set the char size for the current face." (font font) (size :unsigned-int) (res :unsigned-int))
(defcfun ("ftglRenderFont" render-font) :void "Render a string of characters." (font font) (string :string) (mode render-mode))
(defcfun ("ftglCreateTextureFont" create-texture-font) font "Create a specialised FTGLfont object for handling texture-mapped fonts." (file pathname-string))