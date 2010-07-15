;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; task.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Tasks
;;;
(defclass task ()
  ((payload :accessor payload :initarg :payload
            :initform (error "Task must be given a payload function."))
   (exec-time :accessor exec-time))
  (:documentation
  "An task is a  block of code that is executed by an task-queue.
Task firing can be delayed by milliseconds, and they are guaranteed
to not fire until they are 'cooked'.
Tasks can be asynchronously added to an task-queue, which can
remain inactive until execution is ready to start again, and they
will still be 'fired' in the order they were added."   ))

(defvar *task-queue*)
(defmethod initialize-instance :after ((task task) &key
                                       (queue *task-queue*)
                                       (delay 0))
  (setf (exec-time task) (+ delay (now)))
  (push-task task queue))

;; TODO: FORK was simplified. I may need to add looping to it again. Keep it like this for now.
(defmacro fork ((&key queue delay (task-class 'task)) &body body)
  "Turns BODY into a function to be added as a payload to an =task= object, which
   will be delayed by DELAY milliseconds, and added to QUEUE."
  `(make-instance ,task-class
                  :payload (lambda () ,@body)
                  ,@(when queue `(:queue ,queue))
                  ,@(when delay `(:delay ,delay))))

(defmacro with-task-queue (queue &body body)
  `(let ((*task-queue* ,queue))
     ,@body))

(defgeneric execute-task (task)
  (:documentation "Executes a standard task. Nothing fancy, just a funcall.")
  (:method ((task task))
    (funcall (payload task))))

(defgeneric cookedp (task)
  (:documentation "Simply checks that it doesn't shoot its load prematurely.")
  (:method ((task task))
    (let  ((time-difference (- (exec-time task) (now))))
      (when (<= time-difference 0)
        t))))

;;;
;;; Task-queue
;;;
(defclass task-queue ()
  ((queue :initform (make-priority-queue :key #'exec-time) :accessor queue))
  (:documentation
   "An task queue is a container for tasks. Tasks are inserted
into it and automatically sorted according to the task's execution time. The queue
works like a min-priority queue. The top task can be peeked at, or popped."))

;;; - Most of these functions wrap around the priority queue API in util/priority-queue.lisp
;;;   It is pretty much a standard by-the-book min-priority-queue, with items sorted by
;;;   #'exec-time.
(defgeneric push-task (task queue)
  (:method ((task task) (queue task-queue))
    (priority-queue-insert (queue queue) task)
    t))

(defgeneric peek-next-task (queue)
  (:method ((queue task-queue))
    (priority-queue-minimum (queue queue))))

(defgeneric pop-next-task (queue)
  (:method ((queue task-queue))
    (priority-queue-extract-minimum (queue queue))))

(defgeneric task-available-p (queue)
  (:documentation "Simply peeks to see if there's an task in the queue, and if it's cooked.")
  (:method ((queue task-queue))
    (when (and (peek-next-task queue)
               (cookedp (peek-next-task queue)))
      t)))

(defgeneric clear-tasks (queue)
  (:method ((queue task-queue))
    (setf (queue queue) (make-priority-queue :key #'exec-time))))

(defgeneric process-next-task (queue)
  (:method ((queue task-queue))
    (when (task-available-p queue)
      (execute-task (pop-next-task queue)))))

(defgeneric process-cooked-tasks (queue)
  (:method ((queue task-queue))
    (loop
       while (task-available-p queue)
       do (process-next-task queue))))
