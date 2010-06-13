;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; resources.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Generic resources prototype
;;;
(defclass resource () ()
  (:documentation
   "A resource is an object with some aspect that needs to
be (or should be) manually loaded and unloaded. Usually, this
means things like images or music, which need to be freed
from memory under some circumstances. A resource object should
know everything necessary in order to load whatever it is
managing into and out of memory (it should be ready to respond
to LOAD-RESOURCE and UNLOAD-RESOURCE)."))

(defclass file-resource (resource)
  ((filepath :reader filepath :initarg :filepath
             :initform (error "File resources MUST have a filepath.")))
  (:documentation "File resources are resources that get loaded from files."))

(defgeneric load-resource (resource)
  (:documentation
   "Loads the resource's data into memory, activating it."))

(defgeneric unload-resource (resource)
  (:documentation
   "Unloads the resource's data from memory,
handling any freeing that needs to happen"))

(defgeneric loadedp (resource)
  (:documentation
   "Is the resource currently correctly loaded and available for use?
It's worth pointing out that a simple loadedp flag on the object may
not be enough for all possible resource types. It's allowed, and even
advisable, that this function check other things to make sure it is
correctly loaded (such as confirming that the texture ID is valid, in
the case of texture objects)."))

;;;
;;; Resource management
;;;
(defclass resource-manager (resource)
  ((resources :initform nil :accessor resources))
  (:documentation
   "Resource managers can handle multiple resources at a time,
and take care of loading/unloading all of them in big chunks."))

(defgeneric add-resource (resource manager)
  (:documentation "Sets up RESOURCE to be managed by MANAGER.")
  (:method ((resource resource) (manager resource-manager))
    (pushnew resource (resources manager))))

(defgeneric remove-resource (resource manager)
  (:documentation "Ends the management of RESOURCE by MANAGER.")
  (:method ((resource resource) (manager resource-manager))
    (setf (resources manager)
          (delete resource (resources manager)))))

(defgeneric detach-all-resources (manager)
  (:documentation "Removes all resources from MANAGER.")
  (:method ((manager resource-manager))
    (setf (resources manager) nil)))

(defmethod load-resource ((manager resource-manager))
  (mapc #'load-resource (resources manager)))
(defmethod unload-resource ((manager resource-manager))
  (mapc #'unload-resource (resources manager)))

(defvar *resource-manager*)
(defmacro with-resource-manager (manager &body body)
  `(let* ((*resource-manager* ,manager))
     (unwind-protect
          (progn
            ,@body)
       (unload-resource ,manager))))
