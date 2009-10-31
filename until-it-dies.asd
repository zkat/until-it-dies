;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem until-it-dies
  :version "0.1 (unreleased)"
  :description "Until It Dies -- A 2D Game Engine."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (sheeple trivial-garbage cl-opengl cl-openal cl-devil cl-ftgl cl-glfw)
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
             (:file "input")
             (:file "messages")
             (:file "colors")
             (:file "primitives")
             (:file "config")
             (:file "event")
             (:module resources
                      :serial t
                      :components
                      (:file "resources")
                      (:file "textures")
                      (:file "fonts")
                      (:file "sounds"))
             (:file "sprite")
             (:file "engine")))))

(asdf:defsystem until-it-dies.examples
  :version "0.1 (unreleased)"
  :description "Examples for Until It Dies -- A 2D Game Engine"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies)
  :components
  ((:module demo
            :components
            ((:file "testgame")))))
