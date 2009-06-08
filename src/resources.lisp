;; This file is part of Until It Dies

;; resources.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Generic resources prototype
;;;
(defsheep =resource= ()
  ()
  (:documentation
"A resource is an object with some aspect that needs to 
be (or should be) manually loaded and unloaded. Usually, this
means things like images or music, which need to be freed 
from memory under some circumstances. A resource object should
know everything necessary in order to load whatever it is
managing into and out of memory (it should be ready to respond
to LOAD-RESOURCE and UNLOAD-RESOURCE)."))

(defbuzzword load-resource (resource)
  (:documentation
"Loads the resource's data into memory, activating it."))
(defbuzzword unload-resource (resource)
  (:documentation
"Unloads the resource's data from memory, 
handling any freeing that needs to happen"))
(defbuzzword loadedp (resource)
  (:documentation
"Is the resource currently correctly loaded and available for use?
It's worth pointing out that a simple loadedp flag on the object may
not be enough for all possible resource types. It's allowed, and even
advisable, that this buzzword check other things to make sure it is 
correctly loaded (such as confirming that the texture ID is valid, in
the case of =texture= objects.)"))

(defsheep =file-resource= (=resource=)
  ((filepath nil))
  (:documentation "File resources are resources that get loaded from files."))

(defmessage load-resource :before ((resource =resource=))
  "Before actually loading anything, we should make sure that the engine is initialized."	    
  #+nil(unless (initializedp *engine*)
    (error "Cannot load resource ~A: Engine ~A must be initialized." resource *engine*)))

;;;
;;; Resource management
;;;
(defsheep =resource-manager= ()
  ((resources nil :cloneform nil))
  (:documentation
"Resource managers can handle multiple resources at a time,
and take care of loading/unloading all of them in big chunks.
The with-resource-manager macro accepts a =resource-manager= 
object and binds -that- object to the *resource-manager* 
variable within its scope."))

(defmessage attach ((resource =resource=) (manager =resource-manager=))
  (pushnew resource (resources manager)))
(defmessage detach ((resource =resource=) (manager =resource-manager=))
  (with-properties (resources) manager
    (setf resources (delete resource resources))))
(defmessage detach-all ((manager =resource-manager=))
  (setf (resources manager) nil))

(defmessage load-resource ((manager =resource-manager=))
  (mapc #'load-resource (resources manager)))
(defmessage unload-resource ((manager =resource-manager=))
  (mapc #'unload-resource (resources manager)))

(defvar *resource-manager*)
(defmacro with-resource-manager (manager &body body)
  `(let* ((*resource-manager* ,manager))
     (unwind-protect 
	  (progn
	    ,@body)
       (unload-resource ,manager))))

;; We add some :after messages here to handle automatic attachment/detachment.
;; TODO: Keep an eye on this. It might cause some nastyness.
(defmessage load-resource :after ((resource =resource=))
  (attach resource *resource-manager*))
(defmessage unload-resource :after ((resource =resource=))
  (detach resource *resource-manager*))

;;;
;;; Standard textures
;;;
;;; - Textures manage opengl texture objects, and handle their binding.
(defbuzzword bind-texture (texture))
(defbuzzword unbind-texture (texture))

(defsheep =texture= (=resource=)
  ((tex-id nil)
   (target :texture-2d)
   (height 0)
   (width 0)))

(defmessage bind-texture ((texture =texture=))
  (with-properties (tex-id target) texture
    (when (or (null tex-id)
	      (not (gl:texturep tex-id)))
      (load-resource texture))
    (gl:bind-texture target tex-id)))

(defmessage unbind-texture ((texture =texture=))
  (with-properties (target) texture
    (gl:bind-texture target 0)))

(defmessage unload-resource ((texture =texture=))
  (let ((id (tex-id texture)))
    (setf (tex-id texture) nil)
    (handler-case
	(when (and (integerp id)
		   (gl:texturep id))
	 (gl:delete-texture id))
      #+cl-opengl-checks-errors(%gl::opengl-error (c) (values nil c)))))

(defmessage loadedp ((texture =texture=))
  (with-properties (tex-id) texture
    (when (and tex-id
	       (gl:texturep tex-id))
     t)))

;;;
;;; File textures
;;;
(defsheep =file-texture= (=file-resource= =texture=)
  ((filepath "res/lisplogo_alien_256.png"))
  (:documentation "A file texture is loaded from an image file."))

(defmessage load-resource :before ((texture =texture=))
  (when (tex-id texture)
    (unload-resource texture)))

(defmessage load-resource ((texture =file-texture=))
  (ilut:renderer :opengl)
  (ilut:enable :opengl-conv)
  (let ((texture-name (ilut:gl-load-image (filepath texture))))
    (setf (tex-id texture) texture-name)
    (setf (width texture)
	  (il:get-integer :image-width))
    (setf (height texture)
	  (il:get-integer :image-height)))
  ;; (gl:tex-parameter :texture-2d :generate-mipmap t)
  ;; (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (gl:bind-texture :texture-2d 0)
  (il:bind-image 0)
  texture)

(defmessage load-resource :after ((texture =texture=))
  (let ((id (tex-id texture)))
    (finalize texture (lambda ()
			(when (and (integerp id)
				   (gl:texturep id))
			  (gl:delete-texture id))))))

(defun create-texture (filepath)
  (clone (=file-texture=)
	 ((filepath filepath))))

;;;
;;; Fonts
;;;
(defsheep =font= (=file-resource=)
  ((font-pointer nil)
   (size 12)
   (res 20)
   (loadedp nil)
   (filepath "res/example.otf"))
  (:documentation "A font is used by the text-drawing system to draw strings to screen."))

(defun create-font (filepath &key (size 12) (res 20))
  (clone (=font=) ((filepath filepath) (size size) (res res))))

(defvar *font* =font=)

(defmacro with-font (font &body body)
  "Binds *default-font* to FONT within BODY."
  `(let ((*font* ,font))
     ,@body))

(defmessage load-resource :before ((font =font=))
  (when (font-pointer font)
    (unload-resource font)))

(defmessage load-resource ((font =font=))
  (setf (font-pointer font)
	(ftgl:create-texture-font (filepath font)))
  (ftgl:set-font-face-size (font-pointer font)
			   (size font)
			   (res font))
  (setf (loadedp font) t)
  font)

(defmessage load-resource :after ((font =font=))
  (let ((ptr (font-pointer font)))
    (finalize font (lambda ()
		     (ftgl:destroy-font ptr)))))

;; Anytime we change a font's dimensions while *engine* is initialized, we should reload it.
(defmessage (setf size) :after (new-size (font =font=))
  (declare (ignore new-size))
  (when (initializedp *engine*)
   (load-resource font)))

(defmessage (setf res) :after (new-res (font =font=))
  (declare (ignore new-res))
  (when (initializedp *engine*)
    (load-resource font)))

(defmessage unload-resource ((font =font=))
  (ftgl:destroy-font (font-pointer font))
  (setf (font-pointer font) nil)
  (setf (loadedp font) nil)
  font)
