(asdf:defsystem until-it-dies.graphics
  :version "0.1"
  :description "Until It Dies -- Fancy graphics module."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (until-it-dies.base zpb-ttf cl-glu cl-devil)
  :components
  ((:module "src"
            :components
            ((:module "graphics"
                      :components
                      ((:file "ftgl")
                       (:file "textures")
                       (:file "font-backend")
                       (:file "fonts" :depends-on ("font-backend" "ftgl"))
                       (:file "font-format" :depends-on ("fonts"))
                       (:file "drawable" :depends-on ("font-format" "fonts" "textures"))))))))
