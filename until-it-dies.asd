;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem until-it-dies
  :version "0.1 (unreleased)"
  :description "Until It Dies -- A 2D Game Engine."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (until-it-dies.base until-it-dies.graphics))

(cl:defpackage until-it-dies.examples.resource-info
  (:export :*resource-directory*))

(cl:defvar until-it-dies.examples.resource-info:*resource-directory*
  (print (merge-pathnames "examples/res/" *load-truename*)))

(asdf:defsystem until-it-dies.examples
  :version "0.1 (unreleased)"
  :description "Examples for Until It Dies -- A 2D Game Engine"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (until-it-dies)
  :components
  ((:module "demo" :components
            ((:file "testgame")))))
