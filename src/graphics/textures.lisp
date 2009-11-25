;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; textures.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Standard textures
;;;
;;; - Textures manage opengl texture objects, and handle their binding.
(defproto =texture= (=resource=)
  ((tex-id nil)
   (target :texture-2d)
   (height 0)
   (width 0)))

(defreply bind-texture ((texture =texture=))
  (with-properties (tex-id target) texture
    (when (or (null tex-id)
              (not (gl:texturep tex-id)))
      (load-resource texture))
    (gl:bind-texture target tex-id)))

(defreply unbind-texture ((texture =texture=))
  (with-properties (target) texture
    (gl:bind-texture target 0)))

(defreply unload-resource ((texture =texture=))
  (let ((id (tex-id texture)))
    (setf (tex-id texture) nil)
    (handler-case
        (when (and (integerp id)
                   (gl:texturep id))
          (gl:delete-texture id))
      #+cl-opengl-checks-errors(%gl::opengl-error (c) (values nil c)))))

(defreply loadedp ((texture =texture=))
  (with-properties (tex-id) texture
    (when (and tex-id
               (gl:texturep tex-id))
      t)))

;;;
;;; File textures
;;;
(defproto =file-texture= (=file-resource= =texture=) ()
  (:documentation "A file texture is loaded from an image file."))

(defreply load-resource :before ((texture =texture=))
  (when (tex-id texture)
    (unload-resource texture)))

(defun check-devil-error ()
  (let ((err (uid-il:get-error)))
    (unless (eq err :no-error)
      (error "Texture error: ~A" err))))

(defreply load-resource ((texture =file-texture=))
  (uid-ilut:renderer :opengl)
  (uid-ilut:enable :opengl-conv)
  (let* ((texture-name (uid-ilut:gl-load-image (namestring (filepath texture)))))
    (check-devil-error)
    (setf (tex-id texture) texture-name)
    (setf (width texture)
          (uid-il:get-integer :image-width))
    (setf (height texture)
          (uid-il:get-integer :image-height)))
  (check-devil-error)
  (gl:tex-parameter :texture-2d :generate-mipmap t)
  ;;  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (gl:bind-texture :texture-2d 0)
  (uid-il:bind-image 0)
  (check-devil-error)
  texture)

(defreply load-resource :after ((texture =texture=))
  (let ((id (tex-id texture)))
    (finalize texture (lambda ()
                        (when (and (integerp id)
                                   (gl:texturep id))
                          (gl:delete-texture id))))))

(defreply create ((texture =file-texture=) &key filepath)
  (defobject (=file-texture=)
      ((filepath filepath))))

(defun create-texture (filepath)
  (defobject (=file-texture=)
      ((filepath filepath))))

