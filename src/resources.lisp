(in-package :until-it-dies)

(defsheep =resource= () ())

(defbuzzword load-texture (texture))
(defbuzzword bind-texture (texture))
(defbuzzword unload-texture (texture))
(defbuzzword reload-texture (texture))
(defbuzzword loadedp (texture))

(defsheep =texture= ()
  ((tex-id nil)
   (target :texture-2d)))

(defmessage bind-texture ((texture =texture=))
  (unless (tex-id texture)
    (load-texture texture))
  (gl:bind-texture (target texture) (tex-id texture)))

(defmessage unload-texture ((texture =texture=))
  (let* ((id (tex-id texture)))
    (setf (tex-id texture) nil)
    (handler-case
	(gl:delete-textures (list id))
      #+cl-opengl-checks-errors(%gl::opengl-error (c) (values nil c)))))

(defsheep =file-texture= (=texture=)
  ((filepath nil)))

(defmessage load-texture ((texture =file-texture=))
  (when (tex-id texture)
    (delete-texture texture))
  (setf (tex-id texture)   
	(let ((texture-name (car (gl:gen-textures 1)))
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
	  texture-name)))

(defmessage loadedp ((texture =texture=))
  (when (tex-id texture)
    t))
