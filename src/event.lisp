;; This file is part of yashmup

;; event.lisp
;;
;; Contains the event-queue and event classes, and code to interact with them.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :yashmup)

;;;
;;; Events
;;;

(defsheep =event= ()
  ((payload (lambda () (print "=event='s event fired.")))
   (exec-time 0)))

;; TODO: Figure out a nice way to update FORK
;;
;; (defun make-event (payload 
;; 		   &key (delay 0) start-time
;; 		   (level (current-level *game*))
;; 		   repetitions (repeat-delay 0))
;;   "Generates one or more events that execute PAYLOAD."
;;   (let ((target-frame (or start-frame (current-frame level))))
;;     (push-event (clone (=event=)
;; 		       ((payload payload)
;; 			(exec-frame (+ delay target-frame))))
;; 		level)
;;     (when repetitions
;;       (labels ((recurse (times)
;; 			 (if (<= times 0)
;; 			     nil
;; 			     (progn
;; 			       (push-event (clone (=event=)
;; 						  ((payload payload)
;; 						   (exec-frame (+ repeat-delay (current-frame level)))))
;; 					   level)
;; 			       (push-event (clone (=event=)
;; 						  ((payload (lambda ()
;; 							      (recurse (1- times))))
;; 						   (exec-frame (+ repeat-delay (current-frame level)))))
;; 					   level)))))
;; 	(recurse repetitions)))))

;; (defmacro fork ((&key level delay repetitions repeat-delay) 
;; 		&body body)
;;   "Turns BODY into one or more event-loop events."
;;   `(make-event (lambda () ,@body)
;; 	       ,@(when level `(:level ,level))
;; 	       ,@(when delay `(:delay ,delay))
;; 	       ,@(when repetitions `(:repetitions ,repetitions))
;; 	       ,@(when repeat-delay `(:repeat-delay ,repeat-delay))))


;;;
;;; Event processing
;;;
(defbuzzword execute-event (event)
  (:documentation "Takes care of executing a particular event."))
(defbuzzword cooked-p (event)
  (:documentation "Is the event ready to fire?"))

;;; Methods
(defmessage execute-event ((event =event=))
  "Executes a standard event. Nothing fancy, just a funcall."
  (funcall (payload event)))

(defmessage cooked-p ((event =event=))
  "Simply checks that it doesn't shoot its load prematurely.."
  (let  ((time-difference (- (exec-time event) (now))))
    (when (<= time-difference 0)
      t)))

;;;
;;; Event-queue
;;;
(defsheep =event-queue= ()
  ((queue (make-priority-queue :key #'exec-time) 
	  :cloneform (make-priority-queue :key #'exec-time))))

;;; Generics
(defbuzzword push-event (event queue)
  (:documentation "Adds EVENT to QUEUE"))
(defbuzzword peek-next-event (queue)
  (:documentation "Peeks at the next event in QUEUE without removing it.
Returns NIL if there are no queued events."))
(defbuzzword pop-next-event (queue)
  (:documentation "Returns the next available event, and removes it from QUEUE.
Returns NIL if there is nothing in the queue."))
(defbuzzword event-available-p (queue)
  (:documentation "Is there a cooked event available?"))
(defbuzzword clear-events (queue)
  (:documentation "Clears all events off the event queue"))
(defbuzzword process-next-event (queue)
  (:documentation "Grabs the next event from QUEUE and executes it."))
(defbuzzword process-cooked-events (queue)
  (:documentation "Processes all cooked events in QUEUE"))

;;; Methods
(defmessage push-event ((event =event=) (queue =event-queue=))
  (priority-queue-insert (queue queue) event)
  t)

(defmessage peek-next-event ((queue =event-queue=))
  (priority-queue-minimum (queue queue)))

(defmessage pop-next-event ((queue =event-queue=))
  (priority-queue-extract-minimum (queue queue)))

(defmessage event-available-p ((queue =event-queue=))
  "Simply peeks to see if there's an event in the queue, and if it's cooked."
  (when (and (peek-next-event queue)
	     (cooked-p (peek-next-event queue)))
    t))

(defmessage clear-events ((queue =event-queue=))
  (setf (queue queue) (make-priority-queue :key #'exec-time)))

(defmessage process-next-event ((queue =event-queue=))
  (when (event-available-p queue)
    (execute-event (pop-next-event queue))))

(defmessage process-cooked-events ((queue =event-queue=))
  (loop
     while (event-available-p queue)
     do (process-next-event queue)))

