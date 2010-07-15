;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem until-it-dies.base
  :version "0.1"
  :description "Until It Dies -- A 2D Game Engine."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (cl-opengl glop alexandria)
  :serial t
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:module "util"
                      :depends-on ("packages")
                      :components
                      ((:file "opengl-hacks")
                       (:file "queue")
                       (:file "priority-queue")
                       (:file "finalizers")
                       (:file "split-sequence")
                       (:file "utils")))
             (:file "base-api":depends-on ("util"))
             (:file "task" :depends-on ("util"))
             (:file "clock" :depends-on ("util" "base-api"))
             (:file "colors" :depends-on ("util" "base-api"))
             (:file "primitives" :depends-on ("colors" "base-api"))
             (:file "resources" :depends-on ("util" "base-api"))
             (:file "view" :depends-on ("util" "base-api"))
             (:file "window" :depends-on ("util" "view" "colors" "base-api"))
             (:file "engine" :depends-on ("util" "clock" "base-api"))
             (:file "simple-game-engine" :depends-on ("engine" "window"))))))

