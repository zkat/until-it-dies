;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage until-it-dies
  (:nicknames :uid)
  (:use :cl :alexandria)
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
   :attach
   :detach
   :detach-all
   :step-engine

   :attach
   :detach
   :detach-all

   ;; events
   :on-update
   :on-draw
   :on-key-down
   :on-key-up
   :on-mouse-down
   :on-mouse-up
   :on-mouse-move
   :on-resize
   :on-expose
   :on-obscured
   :on-unobscured
   :on-focus
   :on-blur
   :on-close

   ;; Clock
   :clock
   :max-times-stored
   :tick
   :limit-fps
   :fps

   ;; Engine
   :engine
   :runningp
   :initializedp
   :time-delta
   :windows

   ;; Views
   :left-edge
   :right-edge
   :bottom-edge
   :top-edge
   :far-edge
   :near-edge
   :zoom
   :view-width
   :view-height
   :zoom-view
   :move-view
   :update-view
   :set-view

   ;; Window
   :window
   :title
   :width
   :height
   :fullscreenp
   :resizablep
   :key-repeat-p
   :mouse-x
   :mouse-y
   :set-gl-window
   :view
   :swap-buffers
   :open-window
   :close-window

   ;; primitives
   :make-color
   :mix-colors
   :red
   :green
   :blue
   :alpha
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
   :draw-points
   :draw-line
   :draw-polygon

   ;; resources
   :resource
   :resource-manager
   :file-resource
   :*resource-manager*
   :load-resource
   :unload-resource
   :loadedp

   ;; Fonts
   :font
   :size
   :res
   :*font*
   :with-font

   ;; Sprites
   :sprite
   :draw
   :draw-at
   :textured
   :height
   :width
   :filepath
   :calculate-tex-coords
   :image
   :animation
   :text
   
   ))
