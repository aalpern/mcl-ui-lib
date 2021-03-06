;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	drawing.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;	Drawing functions for UI-Lib.
;;	with-pen-state and frame-rect-3D from Oodles of Utils.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 4/10/96	- draw-3d-line
;; 4/9/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :quickdraw)
(in-package :ui-lib)

;---------------------------------------------------------------------------
; Utils
;---------------------------------------------------------------------------

(defmacro with-pen-state ((&key pnLoc pnSize pnMode pnPat pnPixPat) &body body)
  (let ((state (gensym)))
    `(rlet ((,state :PenState))
       (require-trap #_GetPenState ,state)
       (unwind-protect
         (progn
           ,@(when pnLoc    `((require-trap #_MoveTo (point-h ,pnLoc) (point-v ,pnLoc))))
           ,@(when pnSize   `((require-trap #_PenSize (point-h ,pnSize) (point-v ,pnSize))))
           ,@(when pnMode   `((require-trap #_PenMode ,pnMode)))
           ,@(when pnPat    `((require-trap #_PenPat ,pnPat)))
           ,@(when pnPixPat `((require-trap #_PenPixPat ,pnPixPat)))
           ,@body)
         (require-trap #_SetPenState ,state)))))

;---------------------------------------------------------------------------
; 3D framing effects
;---------------------------------------------------------------------------

(defun frame-rect-3D (rect frame-width shadow-position)
  ;;Frame's the specified Rect with a 3D look.
  ;;Note: this effect only looks right over patterned or colored backgrounds
  (let* ((%top   (pref rect :Rect.top   ))
         (%left  (pref rect :Rect.left  ))
         (%bot   (pref rect :Rect.bottom))
         (%right (pref rect :Rect.right )))
    (declare (dynamic-extent %right %bot %left %top))
    (with-pen-state (:pnSize (make-point frame-width frame-width)
                             :pnMode #$patCopy
                             :pnLoc (make-point %left (- %bot frame-width)))
      ;;left & top edges
      (#_PenPat (ecase shadow-position
                  (:topLeft *black-pattern*)
                  ((:botright :bottomright) *white-pattern*)))
      (#_LineTo %left %top)
      (#_LineTo (- %right frame-width) %top)

      ;;right & bottom edges
      (#_PenPat (ecase shadow-position
                  (:topLeft *white-pattern*)
                  ((:botright :bottomright) *black-pattern*)))
      (#_LineTo (- %right frame-width) (- %bot frame-width))
      (#_LineTo %left (- %bot frame-width))

      ;;topRight & botLeft corners
      (#_PenPat (ecase shadow-position
                  (:topLeft *black-pattern*)
                  ((:botright :bottomright) *white-pattern*)))
      (#_PenSize 1 1)
      (#_MoveTo (- %right frame-width) (+ %top frame-width -2))
      (dotimes (i (1- frame-width)) (#_Line i 0) (#_Move (- i) -1))
      (#_MoveTo %left (1- %bot))
      (dotimes (i frame-width) (#_Line i 0) (#_Move (- i) -1)))))

(defmacro with-3d-colors ((hilit shadow) &body body)
  `(with-back-color ,hilit
     (with-fore-color ,shadow
       ,@body)))

(defmacro with-standard-colors (&body body)
  `(with-3d-colors (*white-color* $chiseling-gray)
     ,@body))

(defun view-frame-rect-3d (view frame-width shadow-position
                                left &optional top right bot)
  (with-focused-view view
    (ccl::with-rectangle-arg (r left top right bot)
      (frame-rect-3D r frame-width shadow-position))))

; for compatibility
(defmacro frame-rect-3D-with-colors (hilit shadow rect frame-width shadow-position)
  `(with-3d-colors (,shadow ,hilit)
     (frame-rect-3D ,rect ,frame-width ,shadow-position)))

; for compatibility
(defmacro frame-rect-3D-with-standard-colors (rect frame-width shadow-position)
  `(with-3d-colors (*white-color* $chiseling-gray)
     (frame-rect-3D ,rect ,frame-width ,shadow-position)))

;---------------------------------------------------------------------------
; Misc. drawing functions
;---------------------------------------------------------------------------

(declaim (inline draw-line))

(defun draw-string (text left top &key
                           (emboss nil)
                           (emboss-color *white-color*))
  (with-returned-pstrs ((s text))
    (when emboss
      (#_moveto (1+ left) (1+ top))
      (with-fore-color emboss-color
        (#_DrawString s)))
    (#_moveto left top)
    (#_DrawString s)))

(defun draw-line (p1 p2)
  "Draws a line between p1 and p2. Should always be called within a (with-focused-view...
since it does not focuse the view. Appropriate pen settings should be done prior
to calling draw-line as well."
  (require-trap #_moveto :long p1)
  (require-trap #_lineto :long p2))

(defun draw-line-3D (p1 p2 &key (width 1) (direction :down))
  (with-pen-saved
    (#_penpat (case direction
                (:up *white-pattern*)
                (:down *black-pattern*)))
    (draw-line p1 p2)
    (#_penpat (case direction
                (:up *black-pattern*)
                (:down *white-pattern*)))
    (draw-line (add-points p1 (make-point 0 width))
               (add-points p2 (make-point 0 width)))))

(defmacro fast-paint-rect (left &optional top right bot color)
  "A version of PAINT-RECT that does not focus the view -- should only
be called within a WITH-FOCUSED-VIEW."
  `(with-fore-color ,color
    (ccl::with-rectangle-arg (r ,left ,top ,right ,bot) (#_PaintRect r))))

;---------------------------------------------------------------------------
; Misc. drawing functions for views and dialog-items
;---------------------------------------------------------------------------

(defun draw-centered-text (item text &key (emboss nil) (emboss-color *white-color*))
  (when (and (ccl::installed-item-p item)
             (not (equalp text "")))
    (multiple-value-bind (ascent descent max-width leading)
                         (font-info (view-font item))
      (declare (ignore max-width))
      (let* ((textHeight (+ ascent descent leading))
             (width (string-width text (view-font item)))
             (h (point-h (view-size item)))
             (v (point-v (view-size item)))
             (left (truncate (/ (- h width) 2)))
             (top  (truncate (/ (- v textHeight) 2))))
        (with-focused-view item
          (with-fore-color (if (dialog-item-enabled-p item)
                             (or (part-color item :text) *black-color*)
                             $medium-light-gray)
            (draw-string text left (+ top ascent)
                         :emboss emboss :emboss-color emboss-color)))
        ))))

(defun view-fill-rect-with-ppat (view ppat left &optional top right bot)
  (with-focused-view view
    (ccl::with-rectangle-arg (r left top right bot)
      (#_fillcrect r ppat))))

;; End of file drawing.lisp
