(in-package :uid)

;;;
;;; Simple game window
;;;
(defclass simple-game-window (window)
  ((engine :initarg :engine :accessor engine)))

(macrolet ((defev (name (window-name &rest rest-of-lambda-list))
             `(defmethod ,name :after ((,window-name simple-game-window) ,@rest-of-lambda-list)
                (,name (engine ,window-name) ,@rest-of-lambda-list))))
  (defev on-key-down (window keycode keysym string))
  (defev on-key-up (window keycode keysym string))
  (defev on-mouse-down (window button))
  (defev on-mouse-up (window button))
  (defev on-mouse-move (window x y))
  (defev on-resize (window width height))
  (defev on-expose (window))
  (defev on-obscured (window))
  (defev on-unobscured (window))
  (defev on-focus (window))
  (defev on-blur (window))
  (defev on-close (window)))

;;; Simple game engine
(defclass simple-game-engine (base-engine)
  ((clock :accessor clock :initarg :clock)
   (path :accessor path :initform *default-pathname-defaults* :initarg :path)
   (main-window :accessor main-window)))

(defmethod initialize-instance :after ((engine simple-game-engine) &key fps-limit
                                       (height 400) (width 400)
                                       (title "Simple UID Game") (clear-color *black*))
  (setf (clock engine)
        (make-instance 'clock :fps-limit fps-limit)
        
        (main-window engine)
        (make-instance 'simple-game-window
                       :engine engine
                       :height height :width width
                       :title title :clear-color clear-color)))

(defmethod width ((engine simple-game-engine))
  (width (main-window engine)))
(defmethod (setf width) (new-value (engine simple-game-engine))
  (setf (width (main-window engine)) new-value))

(defmethod height ((engine simple-game-engine))
  (height (main-window engine)))
(defmethod (setf height) (new-value (engine simple-game-engine))
  (setf (height (main-window engine)) new-value))

(defmethod clear ((engine simple-game-engine))
  (clear (main-window engine)))

(defmethod close-window ((engine simple-game-engine))
  (close-window (main-window engine)))

(defmethod on-draw :before ((engine simple-game-engine))
  (let ((window (main-window engine)))
    (when (openp window)
      (on-draw window))))
(defmethod on-draw ((engine simple-game-engine))
  (values))

(defmethod on-update :before ((engine simple-game-engine) dt)
  (let ((window (main-window engine)))
    (when (openp window)
      (on-update window dt))))
(defmethod on-update ((engine simple-game-engine) dt)
  (declare (ignore dt))
  (values))

(defmethod step-engine :before ((engine simple-game-engine))
  (tick (clock engine)))

(defmethod step-engine ((engine simple-game-engine))
  (on-update engine (time-delta (clock engine)))
  (on-draw engine))

(defmethod init :after ((engine simple-game-engine))
  (init (main-window engine)))

(defmethod teardown :after ((engine simple-game-engine))
  (teardown (main-window engine)))

(defmethod run ((engine simple-game-engine))
  (loop while (openp (main-window engine))
       do (continuable (step-engine engine))))
