;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage #:until-it-dies
  (:nicknames #:uid)
  (:use #:cl #:sheeple #:until-it-dies.resource-info)
  (:export

   ;; time
   :now
   :time-difference
   ;; text
   :out
   :build-string

   ;; core
   :init
   :teardown
   :run
   :update
   :draw

   ;; events
   :key-up
   :key-down
   :key-down-p
   :mouse-up
   :mouse-down
   :mouse-move
   :window-resized
   :idle

   ;; Engine
   :=engine=
   :*engine*
   :with-engine
   :runningp
   :initializedp
   :dt
   :event-queue
   :resource-manager
   :default-font
   :clear-color
   :pausedp
   :mouse-x
   :mouse-y
   :window-width
   :window-height
   :title
   :fps

   ;; primitives
   :make-color
   :mix-colors
   :*color*
   :with-color
   :*black*
   :*white*
   :*magenta*
   :*red*
   :*green*
   :*blue*
   :*yellow*
   :*orange*
   :*brown*
   :make-point
   :point-x
   :point-y
   :point-z
   :draw-rectangle
   :draw-circle
   :draw-triangle
   :draw-quad
   :draw-point
   :draw-line
   :draw-polygon

   ;; resources
   :=resource=
   :=resource-manager=
   :*resource-manager*
   :*resource-directory*
   :=file-resource=
   :load-resource
   :unload-resource
   :loadedp

   ;; textures
   :=texture=
   :=file-texture=
   :bind-texture
   :unbind-texture
   :create-texture

   ;; sounds
   :=sound=
   :=file-sound=
   :source-position
   :source-velocity
   :source-direction
   :sound-state
   :play
   :stop
   :pause
   :rewind

   ;; fonts
   :=font=
   :create-font
   :*font*
   :with-font
   :size
   :res

   ;; sprites
   :draw-sprite
   :create-image
   :width
   :height
   :filepath
   :create-animation
   :num-frames
   :frame-delay
   :frame-width
   :frame-height
   :animation-type

   ;; events
   :*event-queue*
   :fork
   :with-event-queue
   ))
