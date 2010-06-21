(asdf:defsystem until-it-dies.graphics
  :version "0.1"
  :description "Until It Dies -- Fancy graphics module."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (until-it-dies.base zpb-ttf cl-glu)
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