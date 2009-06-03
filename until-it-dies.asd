(asdf:defsystem until-it-dies
  :version "0"
  :description "Until It Dies - a CL 2d game engine."
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "BSD-style"
  :depends-on (cl-opengl cl-devil cl-ftgl lispbuilder-sdl lispbuilder-sdl-mixer sheeple)
  :long-description "Until It Dies is based on the code developed in Yashmup, with some improvements,
                     including opengl-graphics, and Sheeple as an object system."
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")
	     (:module util
		      :serial t
		      :components
		      ((:file "trivial-garbage")
		       (:file "opengl-hacks")
		       (:file "priority-queue")
		       (:file "utils")))
	     (:file "config")
	     (:file "event")
	     (:file "resources")
	     (:file "engine")
	     (:file "screen")
	     (:file "component")))))



