;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	view-mixins.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;	Miscellaneous mixin classes which provide various functionality
;;	for views.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 4/9/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;;=========================================================================
;; Graphic Item Mixin - this lets clicks pass through to whatever view is
;;	underneath
;;=========================================================================

(defclass graphic-item-mixin () ())

(defmethod point-in-click-region-p ((item graphic-item-mixin) point)
  (declare (ignore point))
  nil)

;;=========================================================================
;; Button Mixin - a mixin to provide push-button functionality
;;=========================================================================

(defclass button-mixin (ccl::default-button-mixin) ())

(defgeneric hilite-view (view hilit-flag))
(defmethod  hilite-view (view hilit-flag)
  (declare (ignore view hilit-flag)))

(defmethod button-track ((di button-mixin) initial-mouse-position)
  ;;returns t or nil indicating if the mouse was realeased in the button
  (let ((inverted (point-in-click-region-p di initial-mouse-position)))
    (when inverted (hilite-view di t))
    (loop
      (unless (#_WaitMouseUp)
        (when inverted (hilite-view di nil))
        (return inverted))
      (if (point-in-click-region-p
           di (view-mouse-position (view-container di)))
        (unless inverted (hilite-view di t) (setf inverted t))
        (when inverted (hilite-view di nil) (setf inverted nil))))))

(defmethod view-click-event-handler ((di button-mixin) where)
  (when (with-focused-view (view-container di)
          (button-track di where))
    (call-next-method)))


;; End of file view-mixins.lisp
