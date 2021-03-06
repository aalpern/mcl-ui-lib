;; -*- mode:lisp; package:ui -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	views.lisp
;; author: 	Adam Alpern
;; created: 	7/12/1996
;;
;;	Custom UI-Lib view classes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 7/12/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui)

;; ----------------------------------------------------------------------
;; pict-view

(defclass pict-view (view)
  ((pict :initarg :pict :initform nil :accessor pict)))

(defmethod initialize-instance :after ((view pict-view) &rest args)
  (declare (ignore args))
  (with-slots (pict) view
    (when (and pict
               (or (numberp pict) 		; it's a PICT ID #
                   (stringp pict)))		; it's a PICT name
        (setf pict (get-resource :|PICT| pict)))
    (when (macptrp pict)			; when we've got a PICT handle
      (set-view-size view 			; size the view to the PICT
                     (subtract-points (rref pict :picture.picFrame.bottomRight)
                                      (rref pict :picture.picFrame.topLeft))))))

(defmethod view-draw-contents ((view pict-view))
  (when (pict view)
    (rlet ((r :rect :topleft 0 :bottomright (view-size view)))
      (require-trap #_EraseRect r)
      (require-trap #_DrawPicture (pict view) r))))

;; ----------------------------------------------------------------------
;; grid-view

(defclass grid-view (view)
  ((grid-back-color :initarg :grid-back-color :initform *white-color*
                    :type fixnum
                    :accessor grid-back-color)
   (grid-fore-color :initarg :grid-fore-color :initform 13421823
                    :type fixnum
                    :accessor grid-fore-color)
   (grid-x :initarg :grid-x :initform 8
           :type fixnum
           :accessor grid-x)
   (grid-y :initarg :grid-y :initform 8
           :type fixnum
           :accessor grid-y)
   (frame :initarg :frame :initform nil :accessor frame)))

(defmethod view-draw-contents ((view grid-view))
  (with-slots (grid-x grid-y grid-fore-color grid-back-color frame) view
    (with-fore-color grid-back-color
      (fill-rect view *black-pattern* #@(0 0) (view-size view)))
    (with-fore-color grid-fore-color
      (frame-rect view #@(0 0) (view-size view))
      (let ((nx (round (view-size-h view) grid-x))
            (ny (round (view-size-h view) grid-y)))
        (#_moveto 0 0)
        (dotimes (x nx)
          (#_moveto (* x grid-x) 0)
          (#_lineto (* x grid-x) (view-size-v view)))
        (#_moveto 0 0)
        (dotimes (y ny)
          (#_moveto 0 (* y grid-y))
          (#_lineto (view-size-h view) (* y grid-y)))))
    (when frame
      (rlet ((r :rect :topleft 0 :bottomright (view-size view)))
        (with-standard-colors
          (frame-rect-3d r 1 :topleft))
        (#_insetrect r 1 1)
        (with-fore-color 0 (#_framerect r))))))

(defclass 3d-grid-view (grid-view 3d-frame-mixin) ())

#|
(make-instance 'color-dialog
  :view-subviews (list (make-instance 'grid-view
                         ;:grid-back-color *white-color*
                         ;:grid-fore-color *purple-color*
                         :view-position #@(10 10)
                         :view-size #@(81 81))))

(make-instance '3d-dialog
  :view-subviews (list (make-instance '3d-grid-view
                         ;:grid-back-color *white-color*
                         ;:grid-fore-color *purple-color*
                         :frame t
                         :view-position #@(10 10)
                         :view-size #@(81 81))))

(make-instance '3d-dialog
  :view-subviews (list (make-instance '3d-grid-view
                         :grid-back-color *black-color*
                         :grid-fore-color *dark-green-color*
                         :frame t
                         :view-position #@(10 10)
                         :view-size #@(81 81))))
|#
