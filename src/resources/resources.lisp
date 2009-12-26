;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; resources.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Generic resources prototype
;;;
(defproto =resource= ()
  ()
  :documentation
  "A resource is an object with some aspect that needs to
be (or should be) manually loaded and unloaded. Usually, this
means things like images or music, which need to be freed
from memory under some circumstances. A resource object should
know everything necessary in order to load whatever it is
managing into and out of memory (it should be ready to respond
to LOAD-RESOURCE and UNLOAD-RESOURCE).")

(defproto =file-resource= =resource=
  (filepath)
  :documentation "File resources are resources that get loaded from files.")

(defreply load-resource :before ((resource =resource=))
  "Before actually loading anything, we should make sure that the engine is initialized."
  #+nil(unless (initializedp *engine*)
    (error "Cannot load resource ~A: Engine ~A must be initialized." resource *engine*)))

;;;
;;; Resource management
;;;
(defproto =resource-manager= ()
  (resources)
  :documentation
  "Resource managers can handle multiple resources at a time,
and take care of loading/unloading all of them in big chunks.
The with-resource-manager macro accepts a =resource-manager=
object and binds -that- object to the *resource-manager*
variable within its scope.")
(defreply init-object :after ((obj =resource-manager=) &key)
  (setf (resources obj) nil))

(defreply attach ((resource =resource=) (manager =resource-manager=))
  (pushnew resource (resources manager)))
(defreply detach ((resource =resource=) (manager =resource-manager=))
  (with-properties (resources) manager
    (setf resources (delete resource resources))))
(defreply detach-all ((manager =resource-manager=))
  (setf (resources manager) nil))

(defreply load-resource ((manager =resource-manager=))
  (mapc #'load-resource (resources manager)))
(defreply unload-resource ((manager =resource-manager=))
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
(defreply load-resource :after ((resource =resource=))
  (attach resource *resource-manager*))
(defreply unload-resource :after ((resource =resource=))
  (detach resource *resource-manager*))

