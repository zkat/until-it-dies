(in-package :opengl)

(export '(texturep
	  delete-texture
	  gen-texture))

(defun texturep (texture-id)
  (%gl:is-texture texture-id))

(defun delete-texture (texture-id)
  (delete-textures (list texture-id)))

(defun gen-texture ()
  (car (gen-textures 1)))
