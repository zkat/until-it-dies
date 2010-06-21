(in-package :uid)

(defclass clock ()
  ((last-time :accessor last-time :initform nil)
   (time-delta :accessor time-delta :initform 0)
   (times :accessor times)
   (next-target-time :accessor next-target-time :initform 0)
   (fps-limit :accessor fps-limit :initform nil :initarg :fps-limit)
   (cumulative-time :initform 0.0 :accessor cumulative-time)))

(defmethod max-times-stored ((clock clock))
  (queue-length (times clock)))

(defmethod (setf max-times-stored) (new-value (clock clock))
  (setf (times clock) (make-queue new-value)))

(defmethod initialize-instance :after ((clock clock) &key (max-times-stored 10))
  (setf (times clock) (make-queue max-times-stored)))

(defgeneric tick (clock)
  (:method :before ((clock clock))
    (when (fps-limit clock)
      (limit-fps clock)))
  (:method ((clock clock))
    (with-accessors ((times times) (last-time last-time)
                     (cumulative-time cumulative-time)
                     (dt time-delta))
        clock
      (let ((now (uid:now)))
        (when last-time
          (let ((time-delta (- now last-time)))
            (setf dt time-delta)
            (unless (zerop time-delta)
              (when (= (queue-count times) (max-times-stored clock))
                (decf cumulative-time (dequeue times)))
              (enqueue time-delta times)
              (incf cumulative-time time-delta))))
        (setf last-time now)))))

(defgeneric limit-fps (clock)
  (:method ((clock clock))
    (let* ((now (now))
           (sleep-time (* 2 (- (next-target-time clock) now))))
      (setf (next-target-time clock)
            (+ (/ (fps-limit clock))
               now))
      (when (plusp sleep-time)
       (sleep (float sleep-time))))))

(defgeneric fps (clock)
  (:method ((clock clock))
    (/ (queue-count (times clock))
       (cumulative-time clock))))

