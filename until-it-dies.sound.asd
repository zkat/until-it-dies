(asdf:defsystem until-it-dies.sound
  :version "0.1"
  :description "Until It Dies -- Sound module."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (until-it-dies.base cl-openal)
  :components
  ((:module "src"
            :components
            ((:module "sound"
                      :components
                      ((:file "sounds")))))))