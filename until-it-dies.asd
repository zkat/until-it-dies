;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem until-it-dies
  :version "0"
  :description "Until It Dies - a 2d game engine."
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (sheeple cl-opengl cl-openal cl-devil cl-ftgl lispbuilder-sdl)
  :long-description "Until It Dies is based on the code developed in Yashmup, with some improvements,
                     including opengl-graphics, and Sheeple as an object system."
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "packages")
             (:module util
                      :serial t
                      :components
                      ((:file "trivial-garbage")
                       (:file "opengl-hacks")
                       (:file "priority-queue")
                       (:file "utils")))
             (:file "messages")
             (:file "primitives")
             (:file "config")
             (:file "event")
             (:file "resources")
             (:file "sprite")
             (:file "engine")
             (:file "resources-late")))))
