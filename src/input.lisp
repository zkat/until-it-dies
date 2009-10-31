;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; input.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :until-it-dies)

;;;
;;; Callbacks for GLFW events
;;;
(cffi:defcallback key-hook :void ((key :int) (action :int))
  "Invokes KEY-DOWN or KEY-UP on the active engine, for control keys."
  (unless (<= key 255)
    (restartable
      (funcall (case action
                 (#.glfw:+press+ 'key-down)
                 (#.glfw:+release+ 'key-up))
               *engine* (translate-control-key key))))) ; TODO: Write this

(cffi:defcallback char-hook :void ((key :int) (action :int))
  "Invokes KEY-DOWN or KEY-UP on the active engine, for character input."
  (restartable
    (funcall (case action
               (#.glfw:+press+ 'key-down)
               (#.glfw:+release+ 'key-up))
             *engine* (code-char key))))

;;;
;;; SDL key/mouse constant translation
;;;
(defparameter *all-keys*
  `((,glfw:+key-up+ :up) (,glfw:+key-down+ :down) (,glfw:+key-left+ :left) (,glfw:+key-right+ :right)
    (65 :A) (66 :B) (67 :C) (68 :D) (69 :E) (70 :F) (71 :G) (72 :H) (73 :I) (74 :J) (75 :K) (76 :L)
    (77 :M) (78 :N) (79 :O) (80 :P) (81 :Q) (82 :R) (83 :S) (84 :T) (85 :U) (86 :V) (87 :W) (88 :X)
    (89 :Y) (90 :Z) (49 :1) (50 :2) (51 :3) (52 :4) (53 :5) (54 :6) (54 :7) (55 :8) (56 :9) (48 :0)
    (,glfw:+key-space+ :space) (,glfw:+key-tab+ :tab) (,glfw:+key-del+ :delete)
    (,glfw:+key-insert+ :insert) (,glfw:+key-lctrl+ :left-ctrl) (,glfw:+key-rctrl+ :right-ctrl)
    (,glfw:+key-lalt+ :left-alt) (,glfw:+key-ralt+ :right-alt) (,glfw:+key-esc+ :escape)
    (,glfw:+key-enter+ :return) (,glfw:+key-backspace+ :backspace) (,glfw:+key-rshift+ :right-shift)
    (,glfw:+key-lshift+ :left-shift) (,glfw:+key-pageup+ :page-up) (,glfw:+key-pagedown+ :page-down)
    (,glfw:+key-end+ :end) (,glfw:+key-home+ :home) (,glfw:+key-kp-add+ :keypad-plus)
    (,glfw:+key-kp-subtract+ :keypad-minus) (,glfw:+key-kp-multiply+ :keypad-multiply)
    (,glfw:+key-kp-divide+ :keypad-divide) (,glfw:+key-kp-decimal+ :keypad-period)
    (,glfw:+key-kp-enter+ :keypad-enter) (,glfw:+key-kp-9+ :keypad-9) (,glfw:+key-kp-8+ :keypad-8)
    (,glfw:+key-kp-7+ :keypad-7) (,glfw:+key-kp-4+ :keypad-4) (,glfw:+key-kp-5+ :keypad-5)
    (,glfw:+key-kp-6+ :keypad-6) (,glfw:+key-kp-1+ :keypad-1) (,glfw:+key-kp-2+ :keypad-2)
    (,glfw:+key-kp-3+ :keypad-3) (,glfw:+key-kp-0+ :keypad-0) (,glfw:+key-f1+ :f1)
    (,glfw:+key-f2+ :f2) (,glfw:+key-f3+ :f3) (,glfw:+key-f4+ :f4) (,glfw:+key-f5+ :f5)
    (,glfw:+key-f6+ :f6) (,glfw:+key-f7+ :f7) (,glfw:+key-f8+ :f8) (,glfw:+key-f9+ :f9)
    (,glfw:+key-f10+ :f10) (,glfw:+key-f11+ :f11) (,glfw:+key-f12+ :f12)))

(defparameter *key-table*
  (let ((table (make-hash-table)))
    (loop for (glfw uid) in *all-keys*
         do (setf (gethash glfw table) uid))
    table))

(defun translate-key (key)
  (let ((translation (gethash key *key-table*)))
    (or translation key)))

(defun translate-key-list (keylist)
  (mapcar #'translate-key
          keylist))
