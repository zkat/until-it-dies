;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; sounds.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Sound
;;;
;; TODO - check for errors when loading/unloading/playing
(defclass sound (resource)
  ((buffer-id :initform nil :accessor buffer-id)
   (source-id :initform nil :accessor source-id)
   (source-position :initform '(0 0 0) :accessor source-position)
   (source-velocity :initform '(0 0 0) :accessor source-velocity)
   (source-direction :initform '(0 0 0) :accessor source-direction)
   (source-relative-p :initform nil :accessor source-relative-p)))

(defmethod load-resource :before ((sound sound))
  (when (or (buffer-id sound)
            (source-id sound))
    (unload-resource sound)))

(defmethod unload-resource ((sound sound))
  (al:delete-source (source-id sound))
  (al:delete-buffer (buffer-id sound))
  (setf (buffer-id sound) nil
        (source-id sound) nil))

(defmethod loadedp ((sound sound))
  (let ((bid (buffer-id sound))
        (sid (source-id sound)))
    (when (and bid (al:bufferp bid)
               sid (al:sourcep sid))
      t)))

(defmethod source-position :after ((sound sound))
  (when (loadedp sound)
    (al:source (source-id sound) :position (source-position sound))))

(defmethod source-velocity :after ((sound sound))
  (when (loadedp sound)
    (al:source (source-id sound) :velocity (source-velocity sound))))

(defmethod source-direction :after ((sound sound))
  (when (loadedp sound)
    (al:source (source-id sound) :direction (source-direction sound))))

;;; File sounds
(defclass file-sound (file-resource sound)
  ())

(defmethod load-resource ((sound file-sound))
  (with-accessors ((buffer-id buffer-id) (source-id source-id)
                   (source-position source-position) (source-velocity source-velocity)
                   (source-direction source-direction) (source-relative-p source-relative-p)
                   (filepath filepath))
      sound
    (setf buffer-id (alut:create-buffer-from-file (namestring (truename filepath))))
    (setf source-id (al:gen-source))
    (al:source source-id :buffer buffer-id)
    (al:source source-id :position source-position)
    (al:source source-id :velocity source-velocity)
    (al:source source-id :direction source-direction)
    (al:source source-id :source-relative source-relative-p))
  sound)

(defgeneric sound-state (sound)
  (:documentation "Returns one of: '(:PLAYING :PAUSED :STOPPED :INITIAL). If sound resource is not
loaded yet, returns NIL.")
  (:method ((sound sound))
    (when (loadedp sound)
      (al:get-source (source-id sound) :source-state))))

(defgeneric play (sound)
  (:method :before ((sound sound))
    (unless (loadedp sound)
      (load-resource sound)))
  (:method ((sound sound))
    (al:source-play (source-id sound))))

(defgeneric stop (sound)
  (:method :before ((sound sound))
    (unless (loadedp sound)
      (load-resource sound)))
  (:method ((sound sound))
    (al:source-stop (source-id sound))))

(defgeneric pause (sound)
  (:method :before ((sound sound))
    (unless (loadedp sound)
      (load-resource sound)))
  (:method ((sound sound))
    (al:source-pause (source-id sound))))

(defgeneric rewind (sound)
  (:method :before ((sound sound))
    (unless (loadedp sound)
      (load-resource sound)))
  (:method ((sound sound))
    (al:source-rewind (source-id sound))))

(eval-when (:load-toplevel :execute)
  (alut:init))
