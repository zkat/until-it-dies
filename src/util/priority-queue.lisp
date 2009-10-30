;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; Copyright 2008 Zach Beane
;;;; Copyright 2008-2009 Kat Marchan

;;;; This file is part of yashmup

;;;; priority-queue.lisp
;;;;
;;;; Implementation of a time-based event queue. It handles events depending on their execution
;;;; time. It's a min-priority queue, so the event with the lowest time until execution (which
;;;; can, and often will be, negative), is at the top of the queue.
;;;;
;;;; Note: This priority queue is directly converted from Xach's. Licensing stuff to come when
;;;;       I find the file again. It's a BSD license.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Priority queue
;;;

(defstruct (priority-queue
             (:conc-name %pqueue-)
             (:constructor %make-priority-queue)
             (:print-function %print-priority-queue))
  contents
  keyfun)

(defun make-priority-queue (&key (key #'identity) (element-type t))
  (let ((contents (make-array 100 :adjustable t
                              :fill-pointer 0
                              :element-type element-type)))
    (%make-priority-queue :keyfun key
                          :contents contents)))

(defmethod %print-priority-queue (object stream print-level)
  (declare (ignore print-level))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~[empty~:;~:*~D item~:P~]"
            (length (%pqueue-contents object)))))

(defun priority-queue-minimum (priority-queue)
  "Return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue)))
    (unless (zerop (length contents))
      (heap-minimum contents))))

(defun priority-queue-extract-minimum (priority-queue)
  "Remove and return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (unless (zerop (length contents))
      (heap-extract-minimum contents :key keyfun :test #'<=))))

(defun priority-queue-insert (priority-queue new-item)
  "Add NEW-ITEM to PRIORITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (heap-insert contents new-item :key keyfun :test #'<=)))

(defun priority-queue-empty-p (priority-queue)
  (zerop (length (%pqueue-contents priority-queue))))

(defun priority-queue-remove (priority-queue item &key (test #'eq))
  "Remove and return ITEM from PRIORITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (let ((i (position item contents :test test)))
      (when i
        (heap-extract contents i :key keyfun :test #'<=)))))


;;;
;;; Heap (for the priority queue)
;;;

(defun heap-parent (i)
  (ash i -1))

(defun heap-left (i)
  (ash i 1))

(defun heap-right (i)
  (1+ (ash i 1)))

(defun heap-size (heap)
  (1- (length heap)))

(defun heapify (heap start &key (key #'identity) (test #'<=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (let ((l (heap-left start))
          (r (heap-right start))
          (size (heap-size heap))
          largest)
      (setf largest (if (and (<= l size)
                             (not (ge (key (aref heap start))
                                      (key (aref heap l)))))
                        l
                        start))
      (when (and (<= r size)
                 (not (ge (key (aref heap largest))
                          (key (aref heap r)))))
        (setf largest r))
      (when (/= largest start)
        (rotatef (aref heap largest) (aref heap start))
        (heapify heap largest :key key :test test)))
    heap))

(defun heap-insert (heap new-item &key (key #'identity) (test #'<=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (when (>= (fill-pointer heap) (length heap))
      (adjust-array heap (+ (heap-size heap) 100)))
    (incf (fill-pointer heap))
    (loop for i = (heap-size heap) then parent-i
       for parent-i = (heap-parent i)
       while (and (> i 0)
      (not (ge (key (aref heap parent-i))
         (key new-item))))
       do (setf (aref heap i) (aref heap parent-i))
       finally (setf (aref heap i) new-item))

    heap))

(defun heap-minimum (heap)
  (unless (zerop (length heap))
    (aref heap 0)))

(defun heap-extract (heap i &key (key #'identity) (test #'<=))
  (when (< (length heap) i)
    (error "Heap underflow"))
  (prog1
      (aref heap i)
    (setf (aref heap i) (aref heap (heap-size heap)))
    (decf (fill-pointer heap))
    (heapify heap i :key key :test test)))

(defun heap-extract-minimum (heap &key (key #'identity) (test #'<=))
  (heap-extract heap 0 :key key :test test))
