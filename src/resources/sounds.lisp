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
(defproto =sound= =resource=
  ((buffer-id nil)
   (source-id nil)))

(defreply load-resource :before ((sound =sound=))
  (when (or (buffer-id sound)
            (source-id sound))
    (unload-resource sound)))

(defreply unload-resource ((sound =sound=))
  (al:delete-source (source-id sound))
  (al:delete-buffer (buffer-id sound))
  (setf (buffer-id sound) nil
        (source-id sound nil)))

(defreply loadedp ((sound =sound=))
  (with-properties (buffer-id source-id) texture
    (when (and buffer-id (al:bufferp buffer-id) 
               source-id (al:sourcep source-id))
      t)))

;;; File sounds
(defproto =file-sound= (=file-resource= =sound=)
  ((filepath "res/sample.wav")))

(defreply load-resource ((sound =file-sound=))
  (with-properties (buffer-id source-id filepath) sound
    (setf buffer-id (alut:create-buffer-from-file (namestring (truename filepath))))
    (setf source-id (al:gen-source))
    (al:source source-id :buffer buffer-id))
  sound)

(defmessage play (sound)
  (:reply ((sound =sound=))
    (al:source-play (source-id sound))))

(defmessage stop (sound)
  (:reply ((sound =sound=))
    (al:source-stop (source-id sound))))

(defmessage pause (sound)
  (:reply ((sound =sound=))
    (al:source-pause (source-id sound))))

(defmessage rewind (sound)
  (:reply ((sound =sound=))
    (al:source-rewind (source-id sound))))

