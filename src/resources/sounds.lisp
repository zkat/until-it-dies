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
(defproto =sound= (=resource=)
  ((buffer-id nil)
   (source-id nil)
   (source-position '(0 0 0))
   (source-velocity '(0 0 0))
   (source-direction '(0 0 0))
   (source-relative-p t)))

(defreply load-resource :before ((sound =sound=))
  (when (or (buffer-id sound)
            (source-id sound))
    (unload-resource sound)))

(defreply unload-resource ((sound =sound=))
  (al:delete-source (source-id sound))
  (al:delete-buffer (buffer-id sound))
  (setf (buffer-id sound) nil
        (source-id sound) nil))

(defreply loadedp ((sound =sound=))
  (with-properties (buffer-id source-id) sound
    (when (and buffer-id (al:bufferp buffer-id)
               source-id (al:sourcep source-id))
      t)))

(defreply source-position :after ((sound =sound=))
  (when (loadedp sound)
    (al:source (source-id sound) :position (source-position sound))))

(defreply source-velocity :after ((sound =sound=))
  (when (loadedp sound)
    (al:source (source-id sound) :velocity (source-velocity sound))))

(defreply source-direction :after ((sound =sound=))
  (when (loadedp sound)
    (al:source (source-id sound) :direction (source-direction sound))))

;;; File sounds
(defproto =file-sound= (=file-resource= =sound=)
  ((filepath "res/sample.wav")))

(defreply load-resource ((sound =file-sound=))
  (with-properties (buffer-id source-id source-position source-velocity
                    source-direction source-relative-p filepath)
      sound
    (setf buffer-id (alut:create-buffer-from-file (namestring (truename filepath))))
    (setf source-id (al:gen-source))
    (al:source source-id :buffer buffer-id)
    (al:source source-id :position source-position)
    (al:source source-id :velocity source-velocity)
    (al:source source-id :direction source-direction)
    (al:source source-id :source-relative source-relative-p))
  sound)

(defmessage play (sound))
(defreply play :before ((sound =sound=))
  (unless (loadedp sound)
    (load-resource sound)))
(defreply play ((sound =sound=))
  (al:source-play (source-id sound)))

(defmessage stop (sound))
(defreply stop :before ((sound =sound=))
  (unless (loadedp sound)
    (load-resource sound)))
(defreply stop ((sound =sound=))
  (al:source-stop (source-id sound)))

(defmessage pause (sound))
(defreply pause :before ((sound =sound=))
  (unless (loadedp sound)
    (load-resource sound)))
(defreply pause ((sound =sound=))
  (al:source-pause (source-id sound)))

(defmessage rewind (sound))
(defreply rewind :before ((sound =sound=))
  (unless (loadedp sound)
    (load-resource sound)))
(defreply rewind ((sound =sound=))
  (al:source-rewind (source-id sound)))
