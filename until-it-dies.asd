;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(cl:defpackage until-it-dies.resource-info
  (:export :*resource-directory*))

(cl:defvar until-it-dies.resource-info:*resource-directory*
  (print (merge-pathnames "res/" *load-truename*)))

(asdf:defsystem until-it-dies
  :version "0.1 (unreleased)"
  :description "Until It Dies -- A 2D Game Engine."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (sheeple cl-opengl cl-openal)
  :long-description "Until It Dies is based on the code developed in Yashmup, with some improvements,
                     including opengl-graphics, and Sheeple as an object system."
  :serial t
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:module "util" :depends-on ("packages")
                      :components
                      ((:file "opengl-hacks")
                       (:file "priority-queue")
                       (:file "glfw")
                       (:file "utils")))
             (:file "input" :depends-on ("util"))
             (:file "messages" :depends-on ("util"))
             (:file "colors" :depends-on ("util"))
             (:file "primitives" :depends-on ("util"))
             (:file "event" :depends-on ("messages"))
             (:module "resources" :depends-on ("util" "messages")
                      :components
                      ((:file "finalizers")
                       (:file "resources" :depends-on ("finalizers"))
                       (:file "devil")
                       (:file "textures" :depends-on ("devil" "resources"))
                       (:file "ftgl")
                       (:file "fonts" :depends-on ("ftgl" "resources"))
                       (:file "sounds" :depends-on ("resources"))))
             (:file "sprite" :depends-on ("messages" "resources"))
             (:file "engine" :depends-on ("event" "sprite" "util"))))))

(asdf:defsystem until-it-dies.examples
  :version "0.1 (unreleased)"
  :description "Examples for Until It Dies -- A 2D Game Engine"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies)
  :components
  ((:module "demo" :components
            ((:file "testgame")))))
