;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Until It Dies

;;;; font-backend.lisp
;;;;
;;;; Low level text drawing tools.

;;;; Based on nklein's code from this code:
;;;; http://nklein.com/2009/12/rendering-text-with-cl-opengl-and-zpb-ttf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :until-it-dies)

(defun nklein-draw-quad (bx1 by1 bx2 by2)
  (gl:with-primitives :quads
    (gl:vertex bx1 by1)
    (gl:vertex bx2 by1)
    (gl:vertex bx2 by2)
    (gl:vertex bx1 by2)))

(defun make-interpolator (ss cc ee)
  (let ((xx (+ ss (* -2 cc) ee))
        (yy (* 2 (- cc ss)))
        (zz ss))
    #'(lambda (tt)
        (+ (* xx tt tt) (* yy tt) zz))))

(defun interpolate (sx sy ex ey int-x int-y &optional (st 0) (et 1))
  (let ((mx (/ (+ sx ex) 2.0))
        (my (/ (+ sy ey) 2.0))
        (mt (/ (+ st et) 2.0)))
    (let ((nx (funcall int-x mt))
          (ny (funcall int-y mt)))
      (let ((dx (- mx nx))
            (dy (- my ny)))
        (when (< 1 (+ (* dx dx) (* dy dy)))
          (interpolate sx sy nx ny int-x int-y st mt)
          (gl:vertex nx ny)
          (interpolate nx ny ex ey int-x int-y mt et))))))

(defun render-glyph (glyph mode)
  (zpb-ttf:do-contours (contour glyph)
    (gl:with-primitives mode
      (zpb-ttf:do-contour-segments (start ctrl end) contour
        (let ((sx (zpb-ttf:x start))
              (sy (zpb-ttf:y start))
              (cx (when ctrl (zpb-ttf:x ctrl)))
              (cy (when ctrl (zpb-ttf:y ctrl)))
              (ex (zpb-ttf:x end))
              (ey (zpb-ttf:y end)))
          (gl:vertex sx sy)
          (when ctrl
            (let ((int-x (make-interpolator sx cx ex))
                  (int-y (make-interpolator sy cy ey)))
              (interpolate sx sy ex ey int-x int-y)))
          (gl:vertex ex ey))))))

(defun render-string (string font-loader fill)
  (loop :for pos :from 0 :below (length string)
     :for cur = (zpb-ttf:find-glyph (aref string pos) font-loader)
     :for prev = nil :then cur
     :do (when prev
           (gl:translate (- (zpb-ttf:kerning-offset prev cur font-loader)
                            (zpb-ttf:left-side-bearing cur))
                         0 0))
         (render-glyph cur (if fill :polygon :line-strip))
         (gl:translate (zpb-ttf:advance-width cur) 0 0)))

(defun draw-string (font-loader string &key (size 48) (filled t))
  (let* ((box (zpb-ttf:string-bounding-box string font-loader :kerning t))
         (bx1 (aref box 0))
         (by1 (aref box 1))
         (bx2 (aref box 2))
         (by2 (aref box 3)))

    (let ((ss (/ size (zpb-ttf:units/em font-loader))))
      (gl:scale ss ss 1))

    ;(gl:translate (/ (- bx1 bx2) 2) (/ (- by1 by2) 2) 0)

    (gl:with-pushed-attrib (:current-bit :color-buffer-bit :line-bit
                                         :hint-bit :stencil-buffer-bit)
      ;; antialias lines
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:enable :line-smooth)
      (gl:hint :line-smooth-hint :nicest)
      (gl:with-pushed-matrix
        (render-string string font-loader nil))

      (when filled
        ;; fill stencil buffer with filled-in-glyph
        (gl:color-mask nil nil nil nil)
        (gl:enable :stencil-test)
        (gl:stencil-mask 1)
        (gl:clear-stencil 0)
        (gl:clear :stencil-buffer-bit)
        (gl:stencil-func :always 1 1)
        (gl:stencil-op :invert :invert :invert)
        (gl:with-pushed-matrix
          (render-string string font-loader t))

        ;; fill in area subject to stencil
        (gl:color-mask t t t t)
        (gl:stencil-func :equal 1 1)

        (nklein-draw-quad bx1 by1 bx2 by2)))))

        ;(draw-quad (make-point bx1 by1)
        ;           (make-point bx2 by1)
        ;           (make-point bx2 by2)
        ;           (make-point bx1 by2))))))
