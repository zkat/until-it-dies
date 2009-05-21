(asdf:defsystem until-it-dies
  :version "0"
  :description "(Shoot It) Until It Dies"
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (cl-opengl lispbuilder-sdl lispbuilder-sdl-image lispbuilder-sdl-mixer sheeple)
  :long-description "Until It Dies is mostly just a rewrite of Yashmup, but Sheeple-based, 
                     and with an opengl drawing engine. It also features a nicer event system,
                     and a better general architecture."
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")
	     (:module util
		      :serial t
		      :components
		      ((:file "utils")
		       (:file "priority-queue")))
	     (:module opengl
		      :serial t
		      :components
		      ((:file "hacks")
		       (:file "opengl")
		       (:file "texture")
		       (:file "particles")))
	     (:file "config")
	     (:file "event")
	     (:file "engine")
	     (:file "screen")
	     (:file "component")))))



