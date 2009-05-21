(in-package :opengl)

(export 'texturep)

(defun texturep (texture-id)
  (%gl:is-texture texture-id))
