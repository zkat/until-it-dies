(asdf:defsystem until-it-dies.examples
  :version "0.1"
  :description "Examples for Until It Dies -- A 2D Game Engine"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (until-it-dies)
  :components
  ((:module "examples" :components
            ((:file "basic")
             (:file "text")
             (:file "image")
             (:file "multi-window")
             (:file "basic-no-subclass")))))
