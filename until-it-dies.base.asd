;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem until-it-dies.base
  :version "0.1 (unreleased)"
  :description "Until It Dies -- A 2D Game Engine."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (cl-opengl cl-glu glop alexandria)
  :serial t
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:module "util"
                      :depends-on ("packages")
                      :components
                      ((:file "opengl-hacks")
                       (:file "priority-queue")
                       (:file "finalizers")
                       (:file "split-sequence")
                       (:file "utils")))
             (:file "colors" :depends-on ("util"))
             (:file "primitives" :depends-on ("colors"))
             (:file "resources" :depends-on ("util"))
             (:file "view" :depends-on ("util"))
             (:file "window" :depends-on ("util" "view"))
             (:file "engine" :depends-on ("util" "window"))))))

(asdf:defsystem until-it-dies.graphics
  :version "0.1 (unreleased)"
  :description "Until It Dies -- Fancy graphics module."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies.base zpb-ttf)
  :components
  ((:module "src"
            :components
            ((:module "graphics"
                      :components
                      ((:file "devil")
                       (:file "textures" :depends-on ("devil"))
                       (:file "font-backend")
                       (:file "fonts" :depends-on ("font-backend"))
                       (:file "font-format" :depends-on ("fonts"))
                       (:file "sprite" :depends-on ("font-format" "textures"))))))))

(asdf:defsystem until-it-dies.sound
  :version "0.1 (unreleased)"
  :description "Until It Dies -- Sound module."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies.base cl-openal)
  :components
  ((:module "src"
            :components
            ((:module "sound"
                      :components
                      ((:file "sounds")))))))