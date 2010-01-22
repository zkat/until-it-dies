;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; font-format.lisp
;;;;
;;;; Wrap and align for fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :until-it-dies)

(defun word-length (font-loader word)
  (if (= (length word) 0)
      0
      (+ (zpb-ttf:advance-width (zpb-ttf:find-glyph (char word 0) font-loader))
         (word-length font-loader (subseq word 1)))))

(defun line-length (font-loader space-width line)
  (if (null line)
      0
      (+ (word-length font-loader (car line)) space-width (line-length font-loader space-width (cdr line)))))

(defun line-height (font-loader)
   (+ (zpb-ttf:ymax (zpb-ttf:find-glyph #\A font-loader)) (zpb-ttf:line-gap font-loader)))

(defun space-width (font-loader)
  (zpb-ttf:advance-width (zpb-ttf:find-glyph #\Space font-loader)))

(defun format-text (text &key (width 100) (height 100) (font *font*) (wrap t) (align :left) (valign :bottom))
  (with-properties ((font-loader font-pointer) (size size)) font
    (let ((unit-width (/ width (scale-factor size font-loader)))
          (unit-height (/ height (scale-factor size font-loader)))
          (line-height (line-height font-loader))
          (space-width (space-width font-loader)))
      (align-text
       (if wrap
           (wrap-height
            (wrap-width (split-sequence #\Space text) unit-width (curry #'word-length font-loader) space-width)
            line-height unit-height)
           (split-sequence #\Space text))
       unit-width unit-height align (curry #'line-length font-loader space-width) valign line-height))))

(defun wrap-width (words line-width word-length space-width)
  (let ((line-list '())
        (line '())
        (width-left line-width))
    (dolist (word words (push (nreverse line) line-list))
      (let ((cur-word-length (funcall word-length word)))
        (cond
          ((> cur-word-length width-left)
           (push (nreverse line) line-list)
           (setq line (list word)
                 width-left (- line-width cur-word-length)))
          (t
           (push word line)
           (setf width-left (- width-left cur-word-length space-width))))))))

(defun wrap-height (lines line-height height)
  (let ((max-lines (truncate height line-height))
        (length (length lines)))
    (if (> length max-lines)
        (nthcdr (- max-lines length) lines)
        lines)))

(defun align-text (text width height align line-width valign line-height)
  (let* ((text-height (* (length text) line-height))
         (min-y (case valign
                  (:bottom 0)
                  (:top (- height text-height))
                  (:middle (truncate (- height text-height) 2)))))
    (loop
         for line in text
         for index from 0
         collect (list (align-line line width align line-width) (+ min-y (* index line-height)) (apply #'string-join " " line)))))

(defun align-line (line width align line-width)
  (case align
    (:left 0)
    (:right (- width (funcall line-width line)))
    (:middle (truncate (- width (funcall line-width line)) 2))))