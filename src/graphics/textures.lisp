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
(defclass texture (resource)
  ((tex-id :accessor tex-id :initform nil)
   (target :initform :texture-2d :accessor target)
   (height :accessor height :initform 0)
   (width :accessor width :initform 0)))

(defgeneric bind-texture (texture)
  (:method ((texture texture))
    (with-accessors ((tex-id tex-id) (target target))
        texture
      ;; TODO - maybe put this in a :before?
      (when (or (null tex-id)
                (not (gl:texturep tex-id)))
        (load-resource texture))
      (gl:bind-texture target tex-id))))

(defgeneric unbind-texture (texture)
  (:method ((texture texture))
    (gl:bind-texture (target texture) 0)))

(defmethod load-resource :before ((texture texture))
  (when (tex-id texture)
    (unload-resource texture)))

(defmethod load-resource :after ((texture texture))
  (let ((id (tex-id texture)))
    (finalize texture (lambda ()
                        (when (and (integerp id)
                                   (gl:texturep id))
                          (gl:delete-texture id))))))

(defmethod unload-resource ((texture texture))
  (let ((id (tex-id texture)))
    (setf (tex-id texture) nil)
    (handler-case
        (when (and (integerp id)
                   (gl:texturep id))
          (gl:delete-texture id))
      #+cl-opengl-checks-errors(%gl::opengl-error (c) (values nil c)))))

(defmethod loadedp ((texture texture))
  (let ((tex-id (tex-id texture)))
    (when (and tex-id (gl:texturep tex-id))
      t)))

(defmethod width :before ((texture texture))
  (unless (loadedp texture)
    (load-resource texture)))

(defmethod height :before ((texture texture))
  (unless (loadedp texture)
    (load-resource texture)))

;;;
;;; File textures
;;;
(defclass file-texture (file-resource texture)
  ()
  (:documentation "A file texture is loaded from an image file."))

(defmethod load-resource ((texture file-texture))
  (ilut:renderer :opengl)
  (ilut:enable :opengl-conv)
  (let* ((texture-name (ilut:gl-load-image (namestring (filepath texture)))))
    (il:check-error)
    (setf (tex-id texture) texture-name)
    (setf (width texture)
          (il:get-integer :image-width))
    (setf (height texture)
          (il:get-integer :image-height)))
  (il:check-error)
  (gl:tex-parameter :texture-2d :generate-mipmap t)
  ;;  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (gl:bind-texture :texture-2d 0)
  (il:bind-image 0)
  (il:check-error)
  texture)
