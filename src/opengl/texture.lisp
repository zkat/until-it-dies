(in-package :until-it-dies)

(defbuzzword create-texture (texture))
(defbuzzword bind-texture (texture))
(defbuzzword delete-texture (texture))
(defbuzzword reload-texture (texture))

(defsheep =texture= ()
  ((name nil)
   (target :texture-2d)))

(defmessage bind-texture ((texture =texture=))
  (gl:bind-texture (target texture) (name texture)))

(defmessage delete-texture ((texture =texture=))
  (when (and (name texture)
	     (has-direct-property-p texture 'name))
    (let* ((name (name texture)))
      (setf (name texture) nil)
      (handler-case
	  (gl:delete-textures (list name))
	#+cl-opengl-checks-errors(%gl::opengl-error (c) (values nil c))))))

(defsheep =file-texture= (=texture=)
  ((filename nil)))

(defmessage load-texture ((texture =file-texture=))
  (unless (name texture)
    (delete-texture texture))
  (setf (name texture)   
	(let ((texture-name (car (gl:gen-textures 1)))
	      (image (sdl-image:load-image (filename texture)))
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

(defsheep =sequence-texture= (=texture=)
  ((width 0)
   (height 0)
   (image-data nil)))

(defmessage load-texture ((texture =sequence-texture=))
  (when (name texture)
    (delete-texture texture))
  (let ((name (car (gl:gen-textures 1)))
	(target (target texture)))
    (gl:bind-texture target name)
    (gl:tex-parameter target :texture-mag-filter :linear)
    (gl:tex-parameter target :generate-mipmap t)
    (gl:tex-parameter target :texture-min-filter :linear-mipmap-linear)
    (gl:tex-image-2d target 0 :rgba (width texture) (height texture)
		     0 :rgba :unsigned-byte (image-data texture))))

(defsheep =sprite= ()
  ((x 0)
   (y 0)
   (width 0)
   (height 0)
   (texture nil)))

(defmessage draw ((sprite =sprite=))
  (with-properties (x y width height texture) 
      sprite
    (unless (name texture)
      (load-texture texture))
    (bind-texture texture)
    (gl:with-primitives :quads
     (rectangle x y width height))))
