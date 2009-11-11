(asdf:defsystem until-it-dies.graphics
  :version "0.1 (unreleased)"
  :description "Until It Dies -- Fancy graphics module."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (until-it-dies.base)
  :components
  ((:module "src"
            :components
            ((:module "graphics"
                      :components
                      ((:file "devil")
                       (:file "textures" :depends-on ("devil"))
                       (:file "ftgl")
                       (:file "fonts" :depends-on ("ftgl"))
                       (:file "sprite" :depends-on ("fonts" "textures"))))))))
