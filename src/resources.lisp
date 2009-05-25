(in-package :until-it-dies)

;;;
;;; Generic resources prototype
;;;
;;; - This doesn't do much right now, but I'll refactor texture stuff out to this later.
(defsheep =resource= () ())

(defbuzzword load-resource (resource)
  (:documentation "Loads the resource's data into memory, activating it."))
(defbuzzword unload-resource (resource)
  (:documentation "Unloads the resource's data from memory, 
                   handling any freeing that needs to happen"))
(defbuzzword loadedp (resource))

;;;
;;; Resource management
;;;
(defsheep =resource-manager= ()
  ((resources nil :cloneform nil)))

(defmessage attach ((resource =resource=) (manager =resource-manager=))
  (pushnew resource (resources manager)))
(defmessage detach ((resource =resource=) (manager =resource-manager=))
  (with-properties (resources) manager
    (setf resources (delete resource resources))))

(defmessage load-resource ((manager =resource-manager=))
  (mapc #'load-resource (resources manager)))
(defmessage unload-resource ((manager =resource-manager=))
  (mapc #'unload-resource (resources manager)))

(defmacro with-resource-manager (manager &body body)
  `(let* ((*resource-manager* ,manager))
     (unwind-protect 
	  (progn
	    ,@body)
       (unload-resource ,manager))))

;;;
;;; Standard textures
;;;
(defbuzzword bind-texture (texture))

(defsheep =texture= (=resource=)
  ((tex-id nil)
   (target :texture-2d)
   (height 0)
   (width 0)))

(defmessage bind-texture ((texture =texture=))
  (with-properties (tex-id target) texture
    (when (or (null tex-id)
	      (not (gl:texturep tex-id)))
      (load-texture texture))
    (gl:bind-texture target tex-id)))

(defmessage unload-resource ((texture =texture=))
  (let ((id (tex-id texture)))
    (setf (tex-id texture) nil)
    (handler-case
	(gl:delete-texture id)
      #+cl-opengl-checks-errors(%gl::opengl-error (c) (values nil c)))))

(defmessage loadedp ((texture =texture=))
  (with-properties (tex-id) texture
    (when (and tex-id
	       (gl:texturep tex-id))
     t)))

;;;
;;; File textures
;;;
(defsheep =file-texture= (=texture=)
  ((filepath nil)))

(defmessage load-resource ((texture =file-texture=))
  (when (tex-id texture)
    (unload-texture texture))
  (prog1 (let ((texture-name (gl:gen-texture))
	       (image (sdl-image:load-image (filepath texture)))
	       (target (target texture)))
	   (gl:bind-texture target texture-name)
	   (gl:tex-parameter target :generate-mipmap t)
	   (gl:tex-parameter target :texture-min-filter :linear-mipmap-linear)
	   (sdl-base::with-pixel (pix (sdl:fp image))
	     (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
				     (1 :luminance)
				     (2 :luminance-alpha)
				     (3 :rgb)
				     (4 :rgba))))
	       (assert (and (= (sdl-base::pixel-pitch pix)
			       (* (sdl:width image) (sdl-base::pixel-bpp pix)))
			    (zerop (rem (sdl-base::pixel-pitch pix) 4))))
	       (gl:tex-image-2d target 0 :rgba
				(sdl:width image) (sdl:height image)
				0
				texture-format
				:unsigned-byte (sdl-base::pixel-data pix))))
	   (setf (width texture) (sdl:width image))
	   (setf (height texture) (sdl:height image))
	   (setf (tex-id texture) texture-name))
    (let ((id (tex-id texture)))
      (finalize texture (lambda ()
			  (when (gl:texturep id)
			    (gl:delete-texture id)))))))
