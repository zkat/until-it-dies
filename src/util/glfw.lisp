(defpackage #:uid-glfw-types
  (:use #:cl #:cffi)
  (:shadow #:boolean #:byte #:float #:char #:string)
  (:export #:enum #:boolean #:bitfield #:byte #:short #:int #:sizei #:ubyte #:ushort #:uint
           #:float #:clampf #:double #:clampd #:void #:uint64 #:int64
           #:intptr #:sizeiptr
           #:handle
           #:char #:string
           #:half))

(in-package #:uid-glfw-types)

(defctype enum :uint32)
(defctype boolean :uint8)
(defctype bitfield :uint32)
(defctype byte :int8)
(defctype short :int16)
(defctype int :int32)
(defctype sizei :int32)
(defctype ubyte :uint8)
(defctype ushort :uint16)
(defctype uint :uint32)
(defctype float :float)
(defctype clampf :float)
(defctype double :double)
(defctype clampd :double)
(defctype void :void)

#-cffi-features:no-long-long
(defctype uint64 :uint64)
#-cffi-features:no-long-long
(defctype int64 :int64)

;; Find a CFFI integer type the same foreign-size as a pointer
(defctype intptr #.(find-symbol (format nil "INT~d" (* 8 (cffi:foreign-type-size :pointer)))
                                (find-package '#:keyword)))
(defctype sizeiptr #.(find-symbol (format nil "INT~d" (* 8 (cffi:foreign-type-size :pointer)))
                                  (find-package '#:keyword)))

(defctype handle :unsigned-int)

(defctype char :char)

(defctype string :string)

(defctype half :unsigned-short) ; this is how glext.h defines it anyway

(defmethod cffi:expand-to-foreign (value (type (eql 'boolean)))
  `(if ,value 1 0))

(defmethod cffi:expand-from-foreign (value (type (eql 'boolean)))
  `(not (= ,value 0)))

(defmethod cffi:expand-to-foreign (value (type (eql 'clampf)))
  `(coerce ,value 'single-float))

(defmethod cffi:expand-to-foreign (value (type (eql 'clampd)))
  `(coerce ,value 'double-float))

(defmethod cffi:expand-to-foreign (value (type (eql 'float)))
  `(coerce ,value 'single-float))

(defmethod cffi:expand-to-foreign (value (type (eql 'double)))
  `(coerce ,value 'double-float))

;; TODO: Maybe we can find/write a converter to a half? Does anyone want it?
;; TODO: Might we want converters to integer types? What would it be? round, or floor (or even ceil)?

(defpackage uid-glfw
  (:use :cl :cffi :uid-glfw-types)
  (:shadowing-import-from #:uid-glfw-types #:boolean #:byte #:float #:char #:string)
  (:shadow #:sleep #:+red-bits+ #:+green-bits+ #:+blue-bits+
           #:+alpha-bits+ #:+stencil-bits+ #:+depth-bits+
           #:+accum-red-bits+ #:+accum-green-bits+ #:+accum-blue-bits+
           #:+accum-alpha-bits+ #:+aux-buffers+ #:+stereo+
           #:cond
           #:enable #:disable)
  (:export #:key/button-state #:key #:+accelerated+ #:+accum-alpha-bits+ #:+accum-blue-bits+
           #:+accum-green-bits+ #:+accum-red-bits+ #:+active+ #:+alpha-bits+
           #:+alpha-map-bit+ #:+auto-poll-events+ #:+aux-buffers+ #:+axes+
           #:+blue-bits+ #:+build-mipmaps-bit+ #:+buttons+ #:+depth-bits+ #:+false+
           #:+fsaa-samples+ #:+fullscreen+ #:+green-bits+ #:+iconified+ #:+infinity+
           #:+joystick-1+ #:+joystick-10+ #:+joystick-11+ #:+joystick-12+ #:+joystick-13+
           #:+joystick-14+ #:+joystick-15+ #:+joystick-16+ #:+joystick-2+ #:+joystick-3+
           #:+joystick-4+ #:+joystick-5+ #:+joystick-6+ #:+joystick-7+ #:+joystick-8+
           #:+joystick-9+ #:+joystick-last+ #:+key-backspace+ #:+key-del+ #:+key-down+
           #:+key-end+ #:+key-enter+ #:+key-esc+ #:+key-f1+ #:+key-f10+ #:+key-f11+
           #:+key-f12+ #:+key-f13+ #:+key-f14+ #:+key-f15+ #:+key-f16+ #:+key-f17+
           #:+key-f18+ #:+key-f19+ #:+key-f2+ #:+key-f20+ #:+key-f21+ #:+key-f22+
           #:+key-f23+ #:+key-f24+ #:+key-f25+ #:+key-f3+ #:+key-f4+ #:+key-f5+
           #:+key-f6+ #:+key-f7+ #:+key-f8+ #:+key-f9+ #:+key-home+ #:+key-insert+
           #:+key-kp-0+ #:+key-kp-1+ #:+key-kp-2+ #:+key-kp-3+ #:+key-kp-4+ #:+key-kp-5+
           #:+key-kp-6+ #:+key-kp-7+ #:+key-kp-8+ #:+key-kp-9+ #:+key-kp-add+ #:+key-kp-decimal+
           #:+key-kp-divide+ #:+key-kp-enter+ #:+key-kp-equal+ #:+key-kp-multiply+
           #:+key-kp-subtract+ #:+key-lalt+ #:+key-last+ #:+key-lctrl+ #:+key-left+
           #:+key-lshift+ #:+key-pagedown+ #:+key-pageup+ #:+key-ralt+ #:+key-rctrl+
           #:+key-repeat+ #:+key-right+ #:+key-rshift+ #:+key-space+ #:+key-special+
           #:+key-tab+ #:+key-unknown+ #:+key-up+ #:+mouse-button-1+ #:+mouse-button-2+
           #:+mouse-button-3+ #:+mouse-button-4+ #:+mouse-button-5+ #:+mouse-button-6+
           #:+mouse-button-7+ #:+mouse-button-8+ #:+mouse-button-last+ #:+mouse-button-left+
           #:+mouse-button-middle+ #:+mouse-button-right+ #:+mouse-cursor+
           #:+no-rescale-bit+ #:+nowait+ #:+opened+ #:+origin-ul-bit+ #:+present+ #:+press+
           #:+red-bits+ #:+refresh-rate+ #:+release+ #:+stencil-bits+ #:+stereo+ #:+sticky-keys+
           #:+sticky-mouse-buttons+ #:+system-keys+ #:+true+ #:+wait+ #:+window+ #:+window-no-resize+
           #:boolean #:broadcast-cond #:close-window #:create-cond #:create-mutex #:create-thread
           #:defcfun+doc #:defcfun+out+doc #:destroy-cond #:destroy-mutex #:destroy-thread
           #:disable #:do-window #:enable #:extension-supported #:free-image #:get-desktop-mode
           #:get-gl-version #:get-joystick-buttons #:get-joystick-param #:get-joystick-pos
           #:get-key #:get-mouse-button #:get-mouse-pos #:get-mouse-wheel #:get-number-of-processors
           #:get-proc-address #:get-thread-id #:get-time #:get-version #:get-video-modes
           #:get-window-param #:get-window-size #:iconify-window #:init #:load-memory-texture-2d
           #:load-texture-2d #:load-texture-image-2d #:lock-mutex #:open-window #:open-window-hint
           #:poll-events #:read-image #:read-memory-image #:restore-window #:set-char-callback
           #:set-key-callback #:set-mouse-button-callback #:set-mouse-pos #:set-mouse-pos-callback
           #:set-mouse-wheel #:set-mouse-wheel-callback #:set-time #:set-window-close-callback
           #:set-window-pos #:set-window-refresh-callback #:set-window-size
           #:set-window-size-callback #:set-window-title #:signal-cond #:sleep #:swap-buffers
           #:swap-interval #:terminate #:unlock-mutex #:wait-cond #:wait-events #:wait-thread
           #:with-init #:with-init-window #:with-lock-mutex #:with-open-window))
(in-package #:uid-glfw)

(defmacro defcfun+out+doc ((c-name lisp-name) return-type (&body args))
  (let ((internal-name (intern (format nil "%~a" lisp-name)))
        (in-arg-names (mapcar #'second (remove-if-not #'(lambda (arg)
                                                          (eql (car arg) :in))
                                                      args)))
        (out-args (mapcar #'cdr (remove-if-not #'(lambda (arg)
                                                   (eql (car arg) :out))
                                               args))))
    `(progn
       (defcfun (,c-name ,internal-name) ,return-type
         ,@(mapcar #'(lambda (arg)
                       (if (eql (car arg) :out)
                           (list (second arg) :pointer)
                           (cdr arg)))
                   args))
       (defun ,lisp-name ,in-arg-names
         (with-foreign-objects ,out-args
           (,internal-name ,@(mapcar #'second args))
           (list ,@(mapcar #'(lambda (arg)
                               `(mem-ref ,(first arg) ',(second arg)))
                           out-args)))))))

;; ECL's DFFI seems to have issues if you don't put the full path in
#+(and unix ecl)
(setf cffi:*foreign-library-directories*
      (list "/usr/local/lib/" "/usr/lib/"))

(cffi:load-foreign-library '(:or
                             #+darwin (:framework "GLFW")
                             (:default "glfw")
                             (:default "libglfw")))

;; Key and button state/action definitions
(defcenum key/button-state
  (:release 0)
  (:press 1))

;; Keyboard key definitions: 8-bit ISO-8859-1 (Latin 1) encoding is used
;; for printable keys (such as A-Z, 0-9 etc), and values above 256
;; represent special (non-printable) keys (e.g. F1, Page Up etc).
(defconstant +key-special+ 256)
(defconstant +key-last+ (+ 256 62))
(cffi:defcenum key
  (:key-unknown -1)
  (:key-space 32)
  (:key-special 256)
  (:key-esc #.(+ 256 1))
  (:key-f1 #.(+ 256 2))
  (:key-f2 #.(+ 256 3))
  (:key-f3 #.(+ 256 4))
  (:key-f4 #.(+ 256 5))
  (:key-f5 #.(+ 256 6))
  (:key-f6 #.(+ 256 7))
  (:key-f7 #.(+ 256 8))
  (:key-f8 #.(+ 256 9))
  (:key-f9 #.(+ 256 10))
  (:key-f10 #.(+ 256 11))
  (:key-f11 #.(+ 256 12))
  (:key-f12 #.(+ 256 13))
  (:key-f13 #.(+ 256 14))
  (:key-f14 #.(+ 256 15))
  (:key-f15 #.(+ 256 16))
  (:key-f16 #.(+ 256 17))
  (:key-f17 #.(+ 256 18))
  (:key-f18 #.(+ 256 19))
  (:key-f19 #.(+ 256 20))
  (:key-f20 #.(+ 256 21))
  (:key-f21 #.(+ 256 22))
  (:key-f22 #.(+ 256 23))
  (:key-f23 #.(+ 256 24))
  (:key-f24 #.(+ 256 25))
  (:key-f25 #.(+ 256 26))
  (:key-up #.(+ 256 27))
  (:key-down #.(+ 256 28))
  (:key-left #.(+ 256 29))
  (:key-right #.(+ 256 30))
  (:key-lshift #.(+ 256 31))
  (:key-rshift #.(+ 256 32))
  (:key-lctrl #.(+ 256 33))
  (:key-rctrl #.(+ 256 34))
  (:key-lalt #.(+ 256 35))
  (:key-ralt #.(+ 256 36))
  (:key-tab #.(+ 256 37))
  (:key-enter #.(+ 256 38))
  (:key-backspace #.(+ 256 39))
  (:key-insert #.(+ 256 40))
  (:key-del #.(+ 256 41))
  (:key-pageup #.(+ 256 42))
  (:key-pagedown #.(+ 256 43))
  (:key-home #.(+ 256 44))
  (:key-end #.(+ 256 45))
  (:key-kp-0 #.(+ 256 46))
  (:key-kp-1 #.(+ 256 47))
  (:key-kp-2 #.(+ 256 48))
  (:key-kp-3 #.(+ 256 49))
  (:key-kp-4 #.(+ 256 50))
  (:key-kp-5 #.(+ 256 51))
  (:key-kp-6 #.(+ 256 52))
  (:key-kp-7 #.(+ 256 53))
  (:key-kp-8 #.(+ 256 54))
  (:key-kp-9 #.(+ 256 55))
  (:key-kp-divide #.(+ 256 56))
  (:key-kp-multiply #.(+ 256 57))
  (:key-kp-subtract #.(+ 256 58))
  (:key-kp-add #.(+ 256 59))
  (:key-kp-decimal #.(+ 256 60))
  (:key-kp-equal #.(+ 256 61))
  (:key-kp-enter #.(+ 256 62))
  (:key-last #.(+ 256 62)))

;; Mouse button definitions
(defcenum mouse-button
  (:mouse-button-1 0)
  (:mouse-button-2 1)
  (:mouse-button-3 2)
  (:mouse-button-4 3)
  (:mouse-button-5 4)
  (:mouse-button-6 5)
  (:mouse-button-7 6)
  (:mouse-button-8 7)
  (:mouse-button-last 7)
  ;; Mouse button aliases
  (:mouse-button-left 0)
  (:mouse-button-right 1)
  (:mouse-button-middle 2))

;; Joystick identifiers
(defcenum joystick-identifier
  (:joystick-1 0)
  (:joystick-2 1)
  (:joystick-3 2)
  (:joystick-4 3)
  (:joystick-5 4)
  (:joystick-6 5)
  (:joystick-7 6)
  (:joystick-8 7)
  (:joystick-9 8)
  (:joystick-10 9)
  (:joystick-11 10)
  (:joystick-12 11)
  (:joystick-13 12)
  (:joystick-14 13)
  (:joystick-15 14)
  (:joystick-16 15)
  (:joystick-last 15))

;;========================================================================
;; Other definitions
;;========================================================================

(defcenum open-window-mode
  ;; glfwOpenWindow modes
  (:window #x00010001)
  (:fullscreen #x00010002))

;; glfwGetWindowParam tokens
(defcenum window-param
  (:opened #x00020001)
  (:active #x00020002)
  (:iconified #x00020003)
  (:accelerated #x00020004)
  (:red-bits #x00020005)
  (:green-bits #x00020006)
  (:blue-bits #x00020007)
  (:alpha-bits #x00020008)
  (:depth-bits #x00020009)
  (:stencil-bits #x0002000a)

;; The following constants are used for both glfwGetWindowParam
;; and glfwOpenWindowHint
  (:refresh-rate #x0002000b)
  (:accum-red-bits #x0002000c)
  (:accum-green-bits #x0002000d)
  (:accum-blue-bits #x0002000e)
  (:accum-alpha-bits #x0002000f)
  (:aux-buffers #x00020010)
  (:stereo #x00020011)
  (:window-no-resize #x00020012)
  (:fsaa-samples #x00020013))

;; glfwEnable/glfwDisable tokens
(defcenum enable/disable
  (:mouse-cursor #x00030001)
  (:sticky-keys #x00030002)
  (:sticky-mouse-buttons #x00030003)
  (:system-keys #x00030004)
  (:key-repeat #x00030005)
  (:auto-poll-events #x00030006))

;; glfwGetJoystickParam tokens
(defcenum joystick-param
  (:present #x00050001)
  (:axes #x00050002)
  (:buttons #x00050003))

;; Time spans longer than this (seconds) are considered to be infinity
(defconstant +infinity+ 100000d0)

(defcfun ("glfwInit" init) boolean)

(defcfun ("glfwTerminate" terminate) :void)

(defcfun+out+doc ("glfwGetVersion" get-version) :void ((:out major :int)
                                                       (:out minor :int)
                                                       (:out rev :int)))

(defmacro with-init (&body forms)
  "Call uid-glfw:init, execute forms and clean-up with uid-glfw:terminate once finished.
This makes a nice wrapper to an application higher-level form.
Signals an error on failure to initialize. Wrapped in a block named uid-glfw:with-init."
  `(if (uid-glfw:init)
       (unwind-protect
            (block with-init ,@forms)
         (uid-glfw:terminate))
       (error "Error initializing glfw.")))

(defcfun ("glfwOpenWindow" %open-window) boolean
  (width :int) (height :int)
  (redbits :int) (greenbits :int) (bluebits :int) (alphabits :int)
  (depthbits :int) (stencilbits :int) (mode open-window-mode))

(declaim (inline open-window))
(defun open-window (width height &key (redbits 0) (greenbits 0) (bluebits 0)
                    (alphabits 0) (depthbits 0) (stencilbits 0) (mode :window))
  (%open-window width height redbits greenbits bluebits alphabits depthbits stencilbits mode))

(defcfun ("glfwOpenWindowHint" %open-window-hint) :void (target window-param) (hint :int))
(defun open-window-hint (target hint)
  (case target
    ((:window-no-resize :stereo)
     (%open-window-hint target (if hint 1 0)))
    (otherwise
     (%open-window-hint target hint))))

(defcfun ("glfwCloseWindow" close-window) :void)

(defmacro with-open-window ((title width height &key (redbits 0) (greenbits 0) (bluebits 0)
                                   (alphabits 0) (depthbits 0) (stencilbits 0) (mode :window))
                            &body forms)
  `(if (%open-window ,width ,height :redbits ,redbits :greenbits ,greenbits
                     :bluebits ,bluebits :alphabits ,alphabits
                     :depthbits ,depthbits :stencilbits ,stencilbits :mode ,mode)
       (unwind-protect
            (block with-open-window
              (uid-glfw:set-window-title ,title)
              ,@forms)
         (when (uid-glfw:get-window-param :opened)
           (close-window)))
       (error "Error initializing glfw window.")))

(defcfun ("glfwSetWindowCloseCallback" set-window-close-callback) :void (cbfun :pointer))

(defcfun ("glfwSetWindowTitle" set-window-title) :void (title :string))

(defcfun ("glfwSetWindowSize" set-window-size) :void (width :int) (height :int))

(defcfun ("glfwSetWindowPos" set-window-pos) :void (x :int) (y :int))

(defcfun ("glfwGetWindowSize" %get-window-size) :void (width :pointer) (height :pointer))
(defun get-window-size ()
  (cffi:with-foreign-objects ((width :int)
                              (height :int))
    (%get-window-size width height)
    (list (mem-ref width :int)
          (mem-ref height :int))))

(defcfun ("glfwSetWindowSizeCallback" set-window-size-callback) :void (cbfun :pointer))

(defcfun ("glfwIconifyWindow" iconify-window) :void)

(defcfun ("glfwRestoreWindow" restore-window) :void)

(defcfun ("glfwGetWindowParam" %get-window-param) :int (param window-param))
(defun get-window-param (param)
  (case param
    ((:opened :active :iconified :accelerated :window-no-resize :stereo)
     (when (= 1 (%get-window-param param)) t))
    (otherwise (%get-window-param param))))

(defcfun ("glfwSwapBuffers" swap-buffers) :void)

(defcfun ("glfwSwapInterval" swap-interval) :void (interval :int))

(defcfun ("glfwSetWindowRefreshCallback" set-window-refresh-callback) :void (cbfun :pointer))

(defcstruct vidmode
  (width :int)
  (height :int)
  (redbits :int)
  (bluebits :int)
  (greenbits :int))

(defcfun ("glfwGetVideoModes" %get-video-modes) :int (list :pointer) (maxcount :int))

(defun get-video-modes (maxcount)
  (with-foreign-object (list 'vidmode maxcount)
    (let ((count (%get-video-modes list maxcount)))
      (loop for i below count
         collecting
         (let ((mode (cffi:mem-aref list 'vidmode i)))
           (list (foreign-slot-value mode 'vidmode 'width)
                 (foreign-slot-value mode 'vidmode 'height)
                 (foreign-slot-value mode 'vidmode 'redbits)
                 (foreign-slot-value mode 'vidmode 'greenbits)
                 (foreign-slot-value mode 'vidmode 'bluebits)))))))

(defcfun ("glfwGetDesktopMode" %get-desktop-mode) :void (mode :pointer))
(defun get-desktop-mode ()
  (with-foreign-object (mode 'vidmode)
    (%get-desktop-mode mode)
    (list (foreign-slot-value mode 'vidmode 'width)
          (foreign-slot-value mode 'vidmode 'height)
          (foreign-slot-value mode 'vidmode 'redbits)
          (foreign-slot-value mode 'vidmode 'greenbits)
          (foreign-slot-value mode 'vidmode 'bluebits))))

(defcfun ("glfwGetKey" get-key) key/button-state (key :int))

(defcfun ("glfwGetMouseButton" get-mouse-button) key/button-state (button mouse-button))

(defcfun+out+doc ("glfwGetMousePos" get-mouse-pos) :void ((:out xpos :int) (:out ypos :int)))

(defcfun ("glfwSetMousePos" set-mouse-pos) :void (xpos :int) (ypos :int))

(defcfun ("glfwGetMouseWheel" get-mouse-wheel) :int)

(defcfun ("glfwSetMouseWheel" set-mouse-wheel) :void (pos :int))

(defcfun ("glfwSetKeyCallback" set-key-callback) :void (cbfun :pointer))
(defcfun ("glfwSetCharCallback" set-char-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMouseButtonCallback" set-mouse-button-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMousePosCallback" set-mouse-pos-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMouseWheelCallback" set-mouse-wheel-callback) :void (cbfun :pointer))

(defcfun ("glfwGetJoystickParam" %get-joystick-param) key/button-state
  (joy :int) (param joystick-param))
(defun get-joystick-param (joystick param)
  (if (eq :present param)
      (when (= 1 (%get-joystick-param joystick param))
        t)
      (%get-joystick-param joystick param)))

(defcfun ("glfwGetJoystickPos" %get-joystick-pos) :int (joy :int) (pos :pointer) (numaxes :int))

(defun get-joystick-pos (joy numaxes)
  (with-foreign-object (pos :float numaxes)
    (let ((numaxes (%get-joystick-pos joy pos numaxes)))
      (loop for i below numaxes collecting (mem-aref pos :float i)))))

(defcfun ("glfwGetJoystickButtons" %get-joystick-buttons) :int (joy :int) (buttons :pointer) (numbuttons :int))
(defun get-joystick-buttons (joy numbuttons)
  (with-foreign-object (buttons :unsigned-char numbuttons)
    (let ((numbuttons (%get-joystick-buttons joy buttons numbuttons)))
      (loop for i below numbuttons collecting (mem-aref buttons :unsigned-char i)))))

(defcfun ("glfwExtensionSupported" extension-supported) boolean (extension :string))

(defcfun ("glfwGetProcAddress" get-proc-address) :pointer (procname :string))

(defcfun+out+doc ("glfwGetGLVersion" get-gl-version) :void ((:out major :int)
                                                            (:out minor :int)
                                                            (:out rev :int)))

(defcfun ("glfwEnable" enable) :void (token enable/disable))
(defcfun ("glfwDisable" disable) :void (token enable/disable))
