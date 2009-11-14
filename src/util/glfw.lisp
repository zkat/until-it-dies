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
  (:export #:+accelerated+ #:+accum-alpha-bits+ #:+accum-blue-bits+
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

(defconstant +false+ 0)
(defconstant +true+ 1)

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
(defconstant +release+ 0)
(defconstant +press+ 1)

;; Keyboard key definitions: 8-bit ISO-8859-1 (Latin 1) encoding is used
;; for printable keys (such as A-Z, 0-9 etc), and values above 256
;; represent special (non-printable) keys (e.g. F1, Page Up etc).
(defconstant +key-unknown+ -1)
(defconstant +key-space+ 32)
(defconstant +key-special+ 256)
(defconstant +key-esc+ (+ +key-special+ 1))
(defconstant +key-f1+ (+ +key-special+ 2))
(defconstant +key-f2+ (+ +key-special+ 3))
(defconstant +key-f3+ (+ +key-special+ 4))
(defconstant +key-f4+ (+ +key-special+ 5))
(defconstant +key-f5+ (+ +key-special+ 6))
(defconstant +key-f6+ (+ +key-special+ 7))
(defconstant +key-f7+ (+ +key-special+ 8))
(defconstant +key-f8+ (+ +key-special+ 9))
(defconstant +key-f9+ (+ +key-special+ 10))
(defconstant +key-f10+ (+ +key-special+ 11))
(defconstant +key-f11+ (+ +key-special+ 12))
(defconstant +key-f12+ (+ +key-special+ 13))
(defconstant +key-f13+ (+ +key-special+ 14))
(defconstant +key-f14+ (+ +key-special+ 15))
(defconstant +key-f15+ (+ +key-special+ 16))
(defconstant +key-f16+ (+ +key-special+ 17))
(defconstant +key-f17+ (+ +key-special+ 18))
(defconstant +key-f18+ (+ +key-special+ 19))
(defconstant +key-f19+ (+ +key-special+ 20))
(defconstant +key-f20+ (+ +key-special+ 21))
(defconstant +key-f21+ (+ +key-special+ 22))
(defconstant +key-f22+ (+ +key-special+ 23))
(defconstant +key-f23+ (+ +key-special+ 24))
(defconstant +key-f24+ (+ +key-special+ 25))
(defconstant +key-f25+ (+ +key-special+ 26))
(defconstant +key-up+ (+ +key-special+ 27))
(defconstant +key-down+ (+ +key-special+ 28))
(defconstant +key-left+ (+ +key-special+ 29))
(defconstant +key-right+ (+ +key-special+ 30))
(defconstant +key-lshift+ (+ +key-special+ 31))
(defconstant +key-rshift+ (+ +key-special+ 32))
(defconstant +key-lctrl+ (+ +key-special+ 33))
(defconstant +key-rctrl+ (+ +key-special+ 34))
(defconstant +key-lalt+ (+ +key-special+ 35))
(defconstant +key-ralt+ (+ +key-special+ 36))
(defconstant +key-tab+ (+ +key-special+ 37))
(defconstant +key-enter+ (+ +key-special+ 38))
(defconstant +key-backspace+ (+ +key-special+ 39))
(defconstant +key-insert+ (+ +key-special+ 40))
(defconstant +key-del+ (+ +key-special+ 41))
(defconstant +key-pageup+ (+ +key-special+ 42))
(defconstant +key-pagedown+ (+ +key-special+ 43))
(defconstant +key-home+ (+ +key-special+ 44))
(defconstant +key-end+ (+ +key-special+ 45))
(defconstant +key-kp-0+ (+ +key-special+ 46))
(defconstant +key-kp-1+ (+ +key-special+ 47))
(defconstant +key-kp-2+ (+ +key-special+ 48))
(defconstant +key-kp-3+ (+ +key-special+ 49))
(defconstant +key-kp-4+ (+ +key-special+ 50))
(defconstant +key-kp-5+ (+ +key-special+ 51))
(defconstant +key-kp-6+ (+ +key-special+ 52))
(defconstant +key-kp-7+ (+ +key-special+ 53))
(defconstant +key-kp-8+ (+ +key-special+ 54))
(defconstant +key-kp-9+ (+ +key-special+ 55))
(defconstant +key-kp-divide+ (+ +key-special+ 56))
(defconstant +key-kp-multiply+ (+ +key-special+ 57))
(defconstant +key-kp-subtract+ (+ +key-special+ 58))
(defconstant +key-kp-add+ (+ +key-special+ 59))
(defconstant +key-kp-decimal+ (+ +key-special+ 60))
(defconstant +key-kp-equal+ (+ +key-special+ 61))
(defconstant +key-kp-enter+ (+ +key-special+ 62))
(defconstant +key-last+ +key-kp-enter+)

;; Mouse button definitions
(defconstant +mouse-button-1+ 0)
(defconstant +mouse-button-2+ 1)
(defconstant +mouse-button-3+ 2)
(defconstant +mouse-button-4+ 3)
(defconstant +mouse-button-5+ 4)
(defconstant +mouse-button-6+ 5)
(defconstant +mouse-button-7+ 6)
(defconstant +mouse-button-8+ 7)
(defconstant +mouse-button-last+ +mouse-button-8+)

;; Mouse button aliases
(defconstant +mouse-button-left+ +mouse-button-1+)
(defconstant +mouse-button-right+ +mouse-button-2+)
(defconstant +mouse-button-middle+ +mouse-button-3+)

;; Joystick identifiers
(defconstant +joystick-1+ 0)
(defconstant +joystick-2+ 1)
(defconstant +joystick-3+ 2)
(defconstant +joystick-4+ 3)
(defconstant +joystick-5+ 4)
(defconstant +joystick-6+ 5)
(defconstant +joystick-7+ 6)
(defconstant +joystick-8+ 7)
(defconstant +joystick-9+ 8)
(defconstant +joystick-10+ 9)
(defconstant +joystick-11+ 10)
(defconstant +joystick-12+ 11)
(defconstant +joystick-13+ 12)
(defconstant +joystick-14+ 13)
(defconstant +joystick-15+ 14)
(defconstant +joystick-16+ 15)
(defconstant +joystick-last+ +joystick-16+)


;;========================================================================
;; Other definitions
;;========================================================================

;; glfwOpenWindow modes
(defconstant +window+ #x00010001)
(defconstant +fullscreen+ #x00010002)

;; glfwGetWindowParam tokens
(defconstant +opened+ #x00020001)
(defconstant +active+ #x00020002)
(defconstant +iconified+ #x00020003)
(defconstant +accelerated+ #x00020004)
(defconstant +red-bits+ #x00020005)
(defconstant +green-bits+ #x00020006)
(defconstant +blue-bits+ #x00020007)
(defconstant +alpha-bits+ #x00020008)
(defconstant +depth-bits+ #x00020009)
(defconstant +stencil-bits+ #x0002000a)

;; The following constants are used for both glfwGetWindowParam
;; and glfwOpenWindowHint
(defconstant +refresh-rate+ #x0002000b)
(defconstant +accum-red-bits+ #x0002000c)
(defconstant +accum-green-bits+ #x0002000d)
(defconstant +accum-blue-bits+ #x0002000e)
(defconstant +accum-alpha-bits+ #x0002000f)
(defconstant +aux-buffers+ #x00020010)
(defconstant +stereo+ #x00020011)
(defconstant +window-no-resize+ #x00020012)
(defconstant +fsaa-samples+ #x00020013)

;; glfwEnable/glfwDisable tokens
(defconstant +mouse-cursor+ #x00030001)
(defconstant +sticky-keys+ #x00030002)
(defconstant +sticky-mouse-buttons+ #x00030003)
(defconstant +system-keys+ #x00030004)
(defconstant +key-repeat+ #x00030005)
(defconstant +auto-poll-events+ #x00030006)

;; glfwWaitThread wait modes
(defconstant +wait+ #x00040001)
(defconstant +nowait+ #x00040002)

;; glfwGetJoystickParam tokens
(defconstant +present+ #x00050001)
(defconstant +axes+ #x00050002)
(defconstant +buttons+ #x00050003)

;; glfwReadImage/glfwLoadTexture2D flags
(defconstant +no-rescale-bit+ #x00000001) ; Only for glfwReadImage
(defconstant +origin-ul-bit+ #x00000002)
(defconstant +build-mipmaps-bit+ #x00000004) ; Only for glfwLoadTexture2D
(defconstant +alpha-map-bit+ #x00000008)

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
  (depthbits :int) (stencilbits :int) (mode :int))

(declaim (inline open-window))
(defun open-window (&optional (width 0) (height 0)
                    (redbits 0) (greenbits 0) (bluebits 0) (alphabits 0)
                    (depthbits 0) (stencilbits 0) (mode +window+))
  (%open-window width height redbits greenbits bluebits alphabits depthbits stencilbits mode))

(defcfun ("glfwOpenWindowHint" open-window-hint) :void (target :int) (hint :int))

(defcfun ("glfwCloseWindow" close-window) :void)

(defmacro with-open-window ((&optional (title "glfw window") (width 0) (height 0)
                                       (redbits 0) (greenbits 0) (bluebits 0) (alphabits 0)
                                       (depthbits 0) (stencilbits 0) (mode +window+))
                            &body forms)
  `(if (%open-window ,width ,height ,redbits ,greenbits ,bluebits ,alphabits 
                     ,depthbits ,stencilbits ,mode)
       (unwind-protect
            (block with-open-window
              (uid-glfw:set-window-title ,title)
              ,@forms)
         (when (= +true+ (uid-glfw:get-window-param uid-glfw:+opened+))
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

(defcfun ("glfwGetWindowParam" get-window-param) :int (param :int))

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

(defcfun ("glfwGetKey" get-key) :int (key :int))

(defcfun ("glfwGetMouseButton" get-mouse-button) :int (button :int))

(defcfun+out+doc ("glfwGetMousePos" get-mouse-pos) :void ((:out xpos :int) (:out ypos :int)))

(defcfun ("glfwSetMousePos" set-mouse-pos) :void (xpos :int) (ypos :int))

(defcfun ("glfwGetMouseWheel" get-mouse-wheel) :int)

(defcfun ("glfwSetMouseWheel" set-mouse-wheel) :void (pos :int))


(defcfun ("glfwSetKeyCallback" set-key-callback) :void (cbfun :pointer))
(defcfun ("glfwSetCharCallback" set-char-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMouseButtonCallback" set-mouse-button-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMousePosCallback" set-mouse-pos-callback) :void (cbfun :pointer))
(defcfun ("glfwSetMouseWheelCallback" set-mouse-wheel-callback) :void (cbfun :pointer))

(defcfun ("glfwGetJoystickParam" get-joystick-param) :int (joy :int) (param :int))

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

(defcfun ("glfwEnable" enable) :void (token :int))
(defcfun ("glfwDisable" disable) :void (token :int))

