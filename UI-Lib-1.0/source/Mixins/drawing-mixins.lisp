;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - 1 . 0
;;
;; file: 	drawing-mixins.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;	Mixin classes for miscellaneous view drawing behavior.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 4/9/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;; ----------------------------------------------------------------------
;; 3D Frame Mixin

(defclass 3d-frame-mixin ()
  ((frame-width  :initarg :frame-width :initform 1 :accessor frame-width)
   (shadow-color :initarg :shadow-color :initform $chiseling-gray
                 :accessor shadow-color)
   (hilit-color  :initarg :hilit-color :initform *white-color*
                 :accessor hilit-color)
   (inset :initarg :inset :initform #@(0 0) :accessor inset)
   (outer-frame :initarg :outer-frame :initform nil :accessor outer-frame)
   (shadow-position :initarg :shadow-position :initform :bottomright :accessor
                    shadow-position)))

(defmethod view-draw-contents :before ((self 3d-frame-mixin))
  (with-slots (hilit-color shadow-color frame-width shadow-position
               inset outer-frame)
              self
    (with-3d-colors (hilit-color shadow-color)
      (view-frame-rect-3d self frame-width shadow-position
                          inset (subtract-points (view-size self) inset)))
    (when outer-frame
      (with-fore-color (or (part-color self :frame) *black-color*)
        (frame-rect self #@(0 0) (view-size self))))))

;; ----------------------------------------------------------------------
;; 3D Field Mixin - like the frame mixin, but draws the frame outside the
;;	bounds of the view, for adding a frame to elements like sequence
;;	dialog items.

(defclass 3D-field-mixin (3D-frame-mixin)
  ((reframe :initarg :reframe :initform t :accessor reframe))
  (:default-initargs :shadow-position :topLeft))

(defmethod view-draw-contents ((self 3D-field-mixin))
  (with-slots (frame-width hilit-color shadow-color shadow-position) self
    (let* ((w (view-window self))
           (hilit (if (typep w '3d-window-mixin)
                    (hilit-color w) hilit-color))
           (shadow (if (typep w '3d-window-mixin)
                     (shadow-color w) shadow-color)))
      (multiple-value-bind (topLeft bottomRight)
                           (view-corners self)
        (rlet ((r :rect :topLeft topLeft
                  :bottomRight bottomRight))
          (#_InsetRect r (- frame-width) (- frame-width))
          (with-3d-colors (hilit shadow)
            (frame-rect-3d r frame-width shadow-position))
          (call-next-method)
          (when (reframe self)
            (#_InsetRect r frame-width frame-width)
            (with-fore-color *black-color*
              (#_FrameRect r))))))))

;; ----------------------------------------------------------------------
;; PPat Mixin - a view with a ppat background

(defclass ppat-mixin ()
  ((ppat :initarg :ppat :initform nil :accessor ppat)))

(defmethod view-draw-contents :before ((self ppat-mixin))
  (when (ppat self)
    (view-fill-rect-with-ppat self #@(0 0) (view-size self))))

;; ----------------------------------------------------------------------
;; Background Mixin - a view with a color/pattern background
;;	Inherits from ppat-mixin - bg color does not draw if view has
;;	a ppat.

(defclass background-mixin (ppat-mixin)
  ((bg-color :initarg :bg-color :initform $window-background-gray :accessor bg-color)
   (bg-pattern :initarg :bg-pattern :initform *black-pattern*
               :accessor bg-pattern)))

(defmethod view-draw-contents :before ((self background-mixin))
  (when (and (bg-color self)
             (not (ppat self)))
    (with-fore-color (bg-color self)
      (fill-rect self (bg-pattern self) #@(0 0) (view-size self)))))

;; ----------------------------------------------------------------------
;; 3D window mixin - mixin with window classes to make a window w/ a
;;	background and a 3D frame.

(defclass 3d-window-mixin (ppat-mixin 3d-frame-mixin)
  ((bg-color :initarg :bg-color :initform $window-background-gray :accessor bg-color))
  (:default-initargs :shadow-color $light-gray4))

(defmethod initialize-instance :after ((w 3d-window-mixin) &rest args)
  (declare (ignore args))
  (unless (ppat w)
    (set-part-color w :content (bg-color w))))

(defmethod (setf bg-color) :after ((w 3d-window-mixin) color)
  (set-part-color w :content color))

;; ----------------------------------------------------------------------
;; Color Icon (cicn) Mixin

(defun cicn-width (icon)
  (rref icon :cicon.iconpmap.bounds.right))

(defun cicn-height (icon)
  (rref icon :cicon.iconpmap.bounds.bottom))

(defclass cicn-mixin ()
  ((cicn :initarg :cicn :initform nil :accessor cicn)
   (cicn-position :initarg :cicn-position :initform :centered
                  :accessor cicn-position)
   (cicn-size :initarg :cicn-size :initform #@(16 16)
              :accessor cicn-size)
   (cicn-align :initarg :cicn-align
               :initform #$atAbsoluteCenter :accessor cicn-align)
   (cicn-transform :initarg :cicn-transform
                   :initform #$ttNone :accessor cicn-transform)))

(defmethod calculate-icon-offset ((view cicn-mixin))
  (let* ((off (subtract-points (view-size view)
                               (cicn-size view)))
         (offset (make-point (truncate (/ (point-h off) 2))
                             (truncate (/ (point-v off) 2)))))
    offset))

(defmethod view-draw-contents :after ((view cicn-mixin))
  (when (cicn view)
    (let ((pos (if (equal (cicn-position view) :centered)
                 (calculate-icon-offset view)
                 (cicn-position view))))
      (with-focused-view view
        (rlet ((r :rect
                  :topleft pos
                  :bottomright (add-points pos (cicn-size view))))
          (#_HLock (cicn view))
          (#_plotCiconHandle r (cicn-align view)
           (cicn-transform view)
           (cicn view))
          (#_HUnLock (cicn view))
          )))))

#+ccl-3 (defmethod dialog-item-disable :before ((item cicn-mixin))
          (setf (cicn-transform item) #$ttDisabled))

#+ccl-3 (defmethod dialog-item-enable :before ((item cicn-mixin))
          (setf (cicn-transform item) #$ttNone))

;; End of file drawing-mixins.lisp
