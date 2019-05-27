;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	dialog-items.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 8/1/96	- added twist-down-dialog-item
;; 7/7/96	- added progress indicators
;; 4/19/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;; ----------------------------------------------------------------------
;; Misc

(defclass cicn-dialog-item (cicn-mixin dialog-item) ())

(defclass cicn-button-dialog-item (button-mixin cicn-mixin dialog-item)
  ((icon-list :initarg :icon-list :initform nil :accessor icon-list)))

(defmethod hilite-view ((item cicn-button-dialog-item) hilite-flag)
  (if hilite-flag
    (setf (cicn item) (second (icon-list item)))
    (setf (cicn item) (first (icon-list item))))
  (view-draw-contents item))


;; ----------------------------------------------------------------------
;; Progress Indicators

(defparameter *progress-indicator-default-done-color*
  (make-color #x4000 #x4000 #x4000))
(defparameter *progress-indicator-default-todo-color*
  (make-color #xcccc #xcccc #xffff))

(defclass progress-indicator (dialog-item)
  ((done-color :initarg :done-color
               :initform *progress-indicator-default-done-color*
               :accessor done-color)
   (todo-color :initarg :todo-color
               :initform *progress-indicator-default-todo-color*
               :accessor todo-color)
   (minimum :initarg :minimum :initform 0   :accessor minimum)
   (maximum :initarg :maximum :initform 100 :accessor maximum)
   (value   :initarg :value   :initform 0   :accessor value)))

(defgeneric progress-set-value (item value))

(defmethod progress-set-value ((item progress-indicator) value)
  (setf (value item) value))

(defmethod progress-set-value :after ((item progress-indicator) value)
  (declare (ignore value))
  (view-focus-and-draw-contents item))

(defclass arc-progress-indicator (progress-indicator) ())

(defmethod view-draw-contents ((item arc-progress-indicator))
  (ccl::with-item-rect (r item)
    (with-fore-color *black-color*
      (#_frameoval r))
    (#_insetrect r 1 1)
    (let* ((length (- (maximum item) (minimum item)))
           (done (- length (value item)))
           done-arc-degree todo-arc-degree)
      (if (zerop done)
        (setf done-arc-degree 0)
        (setf done-arc-degree (truncate (- 360 (* 360 (/ 1 (/ length done)))))))
      (setf todo-arc-degree (- 360 done-arc-degree))
      (with-fore-color (done-color item)
        (#_paintarc r 0 done-arc-degree))
      (with-fore-color (todo-color item)
        (#_paintarc r done-arc-degree todo-arc-degree)))))

;; ----------------------------------------------------------------------
;; window-resize-dialog-item
;;
;; A resize handle, like the standard one, but placeable anywhere
;; in a window, and less obtrusive.

(defclass window-resize-dialog-item (cicn-dialog-item)
  ((resize-offset :initarg :resize-offset :initform #@(0 0) :accessor resize-offset)
   (style :initarg :style :initform :normal :accessor style)))

(defmethod install-view-in-window :after ((item window-resize-dialog-item) w)
  (setf (resize-offset item) (subtract-points (view-size w) (view-position item))))

(defmethod initialize-instance :after ((item window-resize-dialog-item)
                                       &rest initargs)
  (declare (ignore initargs))
  (case (style item)
    (:normal
     (set-view-size item #@(10 10))
     (setf (cicn-size item) #@(10 10))
     (setf (cicn item) (gethash "Resize Handle" *ui-resource-table*)))
    (:small
     (set-view-size item #@(8 8))
     (setf (cicn-size item) #@(8 8))
     (setf (cicn item) (gethash "Small Resize Handle" *ui-resource-table*)))
    (:diagonal
     (set-view-size item #@(10 10))
     (setf (cicn-size item) #@(10 10))
     (setf (cicn item) (gethash "Diagonal Resize Handle" *ui-resource-table*)))))

(defmethod view-click-event-handler ((item window-resize-dialog-item) where)
  (let* ((window (view-window item)))
    (window-grow-event-handler window (local-to-global window where))
    (set-view-position item (subtract-points (view-size window)
                                             (resize-offset item)))
    (erase-rect window #@(0 0) (view-size window))
    (invalidate-view window)))

;; ----------------------------------------------------------------------
;; twist-down-dialog-item

(defclass twist-down-dialog-item (cicn-dialog-item)
  ((state :initarg :state :initform :up :accessor state))
  (:Default-initargs :cicn-size #@(16 16)
    :cicn (get-ui-resource "Triangle Up")))

(defmethod view-default-size ((w twist-down-dialog-item))
  #@(16 16))

(defun init-twist-icons ()
  (defparameter *twist-down-icons* (list (get-ui-resource "Triangle Up/Hilited")
                                         (get-ui-resource "Triangle Middle/Hilited")
                                         (get-ui-resource "Triangle Down/Hilited")
                                         (get-ui-resource "Triangle Down")))
  (defparameter *twist-up-icons*   (list (get-ui-resource "Triangle Down/Hilited")
                                         (get-ui-resource "Triangle Middle/Hilited")
                                         (get-ui-resource "Triangle Up/Hilited")
                                         (get-ui-resource "Triangle Up"))))

(defmethod draw-twist-down-animation ((view twist-down-dialog-item))
  (multiple-value-bind (topleft bottomright)
                       (view-corners view)
    (declare (dynamic-extent topleft bottomright))
    (with-back-color (get-back-color (view-window view))
      (ccl::with-rectangle-arg (r (point-h topleft) (point-v topleft)
                                  (point-h bottomright) (point-v bottomright))
        (dolist (icon *twist-down-icons*)
          (#_EraseRect r)
          (#_PlotCIcon r icon)
          (%stack-block ((ticks 4))
            (#_Delay 3 ticks)))))))

(defmethod draw-twist-up-animation ((view twist-down-dialog-item))
  (multiple-value-bind (topleft bottomright)
                       (view-corners view)
    (declare (dynamic-extent topleft bottomright))
    (with-back-color (get-back-color (view-window view))
      (ccl::with-rectangle-arg (r (point-h topleft) (point-v topleft)
                                  (point-h bottomright) (point-v bottomright))
        (dolist (icon *twist-up-icons*)
          (#_EraseRect r)
          (#_PlotCIcon r icon)
          (%stack-block ((ticks 4))
            (#_Delay 3 ticks)))))))

(defmethod view-click-event-handler ((item twist-down-dialog-item) where)
  (declare (ignore where))
  (let ((fn (dialog-item-action-function item)))
    (case (state item)
      (:up (draw-twist-down-animation item)
           (when fn (funcall fn item))
           (setf (state item) :down)
           (setf (cicn item) (get-ui-resource "Triangle Down")))
      (:down (draw-twist-up-animation item)
             (when fn (funcall fn item))
             (setf (state item) :up)
             (setf (cicn item) (get-ui-resource "Triangle Up"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thing:	color-swatch-mixin
;;		color-swatch-dialog-item
;;		color-swatch-picker-dialog-item
;; author:	Adam Alpern <ala@neural.hampshire.edu>
;; synopsis:	A simple mixin which frames itself and fills itself
;;		with it's color. color-swatch-picker-dialog-item will
;;		open the Apple color picker when clicked, and set it's
;;		color to the user's choice.

(defclass color-swatch-mixin ()
  ((color :initarg :color :initform *dark-green-color* :accessor color)
   (draw-outline :initarg :draw-outline :initform t :accessor draw-outline)
   ))

(defmethod view-draw-contents ((view color-swatch-mixin))
  (rlet ((r :rect :topleft (view-position view)
            :bottomright (add-points (view-position view) (view-size view))))
    (with-fore-color (or (part-color view :body) (color view))
      (#_FillRect r (if (dialog-item-enabled-p view)
                      *black-pattern*
                      *gray-pattern*)))
    (when (draw-outline view)
      (case (draw-outline view)
        (:topleft (with-standard-colors (frame-rect-3d r 1 :topleft)))
        (:botright (with-standard-colors (frame-rect-3d r 1 :botright)))
        (t
         (with-fore-color (or (part-color view :frame) 0)
           (#_framerect r))))
      )))

(defmethod (setf color) :after (color (item color-swatch-mixin))
  "Invalidates item so that the new icon is drawn."
  (declare (ignore color))
  (invalidate-view item t))

(defclass color-swatch-dialog-item (color-swatch-mixin dialog-item) ())
(defclass color-swatch-picker-dialog-item (color-swatch-mixin dialog-item) ())

(defmethod view-click-event-handler ((view color-swatch-picker-dialog-item)
                                     where)
  (declare (ignore where))
  (setf (color view) (user-pick-color :color (color view))))

(defclass banner-dialog-item (color-swatch-mixin 3d-dialog-item)
  ())

(defmethod view-draw-contents :after ((view banner-dialog-item))
  (draw-centered-text view (dialog-item-text view)))

;; ----------------------------------------------------------------------
;; stuff

(defmethod point-in-click-region-p ((item graphic-item-mixin) point)
  (declare (ignore point))
  nil)

(defclass line-dialog-item (graphic-item-mixin dialog-item)
  ((frame-width :initform 1 :accessor frame-width :initarg :frame-width))
  (:default-initargs :frame-width 1))

(defclass horizontal-line-dialog-item (line-dialog-item) ())
(defclass vertical-line-dialog-item (line-dialog-item) ())

(defmethod view-draw-contents ((w line-dialog-item))
  (multiple-value-bind (topLeft bottomRight)
                       (view-corners w)
    (rlet ((r :rect :topLeft topLeft
              :bottomRight bottomRight))
      (#_framerect r))))

(defmethod initialize-instance :after ((item horizontal-line-dialog-item)
                                       &rest args)
  (declare (ignore args))
  (set-view-size item (make-point (point-h (or (view-size item) #@(50 50)))
                                  (frame-width item))))

(defmethod initialize-instance :after ((item vertical-line-dialog-item)
                                       &rest args)
  (declare (ignore args))
  (set-view-size item (make-point (frame-width item))
                 (point-v (or (view-size item) #@(50 50)))))
