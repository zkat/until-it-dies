(in-package :uid)

(defgeneric init (obj))
(defgeneric teardown (obj))

(defgeneric on-draw (obj))
(defgeneric on-update (obj dt))

(defgeneric attach (obj1 obj2))
(defgeneric detach (obj1 obj2))
(defgeneric detach-all (container))
