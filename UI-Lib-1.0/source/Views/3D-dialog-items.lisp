;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	3D-dialog-items.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 7/18/96	- restored draw-default-button-outline
;; 4/9/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;; ----------------------------------------------------------------------
;; Misc

(defclass 3D-dialog-item (background-mixin 3D-frame-mixin dialog-item) ())

(defclass 3D-editable-text-dialog-item (3D-field-mixin editable-text-dialog-item)
  ())

(defclass 3D-table-dialog-item (3D-field-mixin table-dialog-item)
  ())

(defclass 3D-sequence-dialog-item (3D-field-mixin sequence-dialog-item)
  ())

(defclass 3D-scroller (3D-field-mixin ccl::scroller)
  ())

(defmethod view-draw-contents :before ((view table-dialog-item))
  (ccl::with-item-rect (r view)
    (#_EraseRect r)))

;; ----------------------------------------------------------------------
;; 3D button
;;	square => boolean
;;	inset  => nil | integer (frame-width)
;;	frame  => boolean
;;	embossed => nil | t | :emboss | :shadow  (t & :emboss are equivalent)

; color parameters -> fill shadow highlight

(defparameter *3d-button-hilited-colors*
  (list $medium-gray
        (make-color $rgb-8bit-gray9 $rgb-8bit-gray9 $rgb-8bit-gray9)
        (make-color $rgb-8bit-gray3 $rgb-8bit-gray3 $rgb-8bit-gray3)))

(defparameter *3d-button-normal-colors*
  (list $light-gray2
        $medium-gray
        *white-color*))

(defclass 3d-button-dialog-item (ppat-mixin cicn-mixin button-mixin dialog-item)
  ((square :initarg :square :initform nil :accessor square)
   (inset  :initarg :inset  :initform nil :accessor inset)
   (frame  :initarg :frame  :initform t   :accessor frame)
   (embossed :initarg :embossed :initform nil :accessor embossed)
   (hilited :initarg :hilited :initform nil :accessor hilited)
   (colors   :initarg :colors   :accessor colors
             :initform *3d-button-normal-colors*)
   (hilited-colors :initarg :hilited-colors :accessor hilited-colors
                   :initform *3d-button-hilited-colors*)
   (frame-width :initarg :frame-width :initform 1 :accessor frame-width)
   (oval-width  :initarg :oval-width :initform 4 :accessor oval-width)
   (oval-height :initarg :oval-height :initform 4 :accessor oval-height)
   (default-button :initarg :default-button :initform nil :accessor default-button)
   (shadow-position :initarg :shadow-position :initform :bottomright :accessor
                    shadow-position)))

(defmethod press-button ((button 3d-button-dialog-item))
  (with-focused-view button
    (hilite-view button t)
    (view-focus-and-draw-contents button)
    (let ((time (+ 4 (%get-long (%int-to-ptr #x16a)))))
      (ccl::while (< (%get-long (%int-to-ptr #x16a)) time)))
    (hilite-view button nil)
    (view-focus-and-draw-contents button)
    (dialog-item-action button)))

(defmethod hilite-view ((view 3d-button-dialog-item) hilite-flag)
  (cond (hilite-flag
         (setf (shadow-position view) :topleft)
         #+ccl-3(setf (cicn-transform view) #$ttSelected)
         (setf (hilited view) t)
         (view-draw-contents view))
        (t
         (setf (shadow-position view) :bottomright)
         #+ccl-3(setf (cicn-transform view) #$ttNone)
         (setf (hilited view) nil)
         (view-draw-contents view))))

(defmethod view-corners ((item 3d-button-dialog-item))
  (if (default-button-p item)
    (multiple-value-call #'inset-corners #@(-4 -4) (call-next-method))
    (call-next-method)))

(defmethod view-draw-contents ((di 3D-button-dialog-item))
  (let (fill-color highlight-color shadow-color transform)
    (cond ((null (dialog-item-enabled-p di)) 	; dimmed
           (setf fill-color      (first (colors di))
                 shadow-color    nil
                 highlight-color nil
                 transform #$ttDisabled))
          ((hilited di)			; hilited
           (setf fill-color      (first (hilited-colors di))
                 shadow-color    (second (hilited-colors di))
                 highlight-color (third (hilited-colors di))
                 transform #$ttSelected))
          (t 					; normal
           (setf fill-color      (first  (colors di))
                 shadow-color    (second (colors di) )
                 highlight-color (third  (colors di))
                 transform #$ttNone)
           ))
    (setf (cicn-transform di) transform)
    (ccl::with-item-rect (r di)
      (#_eraserect r)
      (when (and (inset di) (square di)) (#_InsetRect r 1 1))
      ;; � Fill the round rect with the color
      (if (ppat di)
        (#_fillcrect r (ppat di))
        (with-fore-color fill-color
          (if (square di)
            (#_PaintRect r)
            (#_PaintRoundRect r (oval-width di) (oval-height di)))))
      ;; � Inset the area in preparation for the drawing of the buttons content
      (when (frame di) (#_InsetRect r 1 1))
      (when (dialog-item-enabled-p di)	; don't draw the 3D effect unless we're enabled
        (with-3d-colors (highlight-color shadow-color)
          (frame-rect-3d r (frame-width di) (shadow-position di))))
      (#_InsetRect r -1 -1)
      (with-fore-color (if (null (dialog-item-enabled-p di))
                         $medium-gray
                         *black-color*)
        ;; � Frame button with a black border or gray if it's dimmed
        (when (frame di)
          (cond ((square di)
                 (#_FrameRect r)
                 (when (and (inset di)
                            (dialog-item-enabled-p di))
                   (#_InsetRect r -1 -1)
                   (let ((in-3d-win (typep (view-window di) '3d-window-mixin)))
                     (with-3d-colors ((if in-3d-win (hilit-color (view-window di)) highlight-color)
                                      (if in-3d-win (shadow-color (view-window di)) shadow-color))
                       (frame-rect-3d r 1 :topleft)))
                   ))
                (t
                 (#_FrameRoundRect r (oval-width di) (oval-height di))))
          ))))
  (draw-centered-text di
                      (dialog-item-text di)
                      :emboss (embossed di)
                      :emboss-color (car (last (if (hilited di)
                                                 (hilited-colors di)
                                                 (colors di)))))
  (ccl::maybe-draw-default-button-outline di))

(defmethod ccl::draw-default-button-outline ((item 3d-button-dialog-item))
  (when (ccl::installed-item-p item)
    (with-focused-dialog-item (item)
      (let ((grayp (not (dialog-item-enabled-p item)))
            (off 5)
            fill-color highlight-color shadow-color)
        (declare (dynamic-extent fill-color highlight-color
                                 shadow-color off grayp))
        (setf fill-color      (first (hilited-colors item))
              shadow-color    (second (hilited-colors item))
              highlight-color (third (hilited-colors item)))
        (without-interrupts
         (ccl::with-item-rect (rect item)
           (#_insetRect rect -3 -3)
           (if grayp

             (with-pen-saved
               (#_pensize 3 3)
               (with-fore-color fill-color
                 (if (square item)
                   (#_framerect rect)
                   (#_frameroundrect rect (+ off (oval-width item))
                    (+ off (oval-height item))))))

             (with-pen-saved
               (#_pensize 3 3)
               (with-fore-color fill-color
                 (if (square item)
                   (#_framerect rect)
                   (#_frameroundrect rect (+ off (oval-width item))
                    (+ off (oval-height item)))))
               (#_pensize 1 1)
               (#_insetrect rect 1 1)
               (with-3d-colors (shadow-color highlight-color)
                 (frame-rect-3d rect 1 :topleft))
               (#_insetrect rect 1 1)
               (with-3d-colors (shadow-color highlight-color)
                 (frame-rect-3d rect 1 :botright))
               (#_insetrect rect -2 -2)
               (with-fore-color 0
                 (if (square item)
                   (#_framerect rect)
                   (#_frameroundrect rect (+ off (oval-width item))
                    (+ off (oval-height item)))))
               ))
           ))))))

;; ----------------------------------------------------------------------
;; 3D Line DI
;;	This is copied from UI-Lib (I), and will change.

(defclass 3D-line-dialog-item (graphic-item-mixin dialog-item)
  ((frame-colors :initarg :frame-colors :initform (list $chiseling-gray
                                                        *white-color*)
                 :accessor frame-colors)
   (frame-width :initform 1 :accessor frame-width :initarg :frame-width)
   (shadow-position :initform :botRight :accessor shadow-position
                    :initarg :shadow-position)
   (3d-p :initarg :3d-p :initform t :accessor 3d-p))
  (:default-initargs :frame-width 1 :shadow-position :topLeft))

(defclass 3D-horizontal-line-dialog-item (3D-line-dialog-item) ())
(defclass 3D-vertical-line-dialog-item (3D-line-dialog-item) ())

(defmethod view-draw-contents ((w 3D-line-dialog-item))
  (with-focused-view w
    (rlet ((r :rect :topLeft 0
              :bottomRight (view-size w)))
      (if (3d-p w)
        (destructuring-bind (shadow hilit)
                            (frame-colors w)
          (with-3d-colors ( hilit shadow)
            (frame-rect-3d r (frame-width w) (shadow-position w))))
        (#_framerect r)))))

(defmethod initialize-instance :after ((item 3D-horizontal-line-dialog-item)
                                       &rest args)
  (declare (ignore args))
  (set-view-size item (make-point (point-h (view-size item))
                                  (if (3d-p item)
                                    (* (frame-width item) 2)
                                    (frame-width item)))))

(defmethod initialize-instance :after ((item 3D-vertical-line-dialog-item)
                                       &rest args)
  (declare (ignore args))
  (set-view-size item (make-point (if (3d-p item) (* (frame-width item) 2)
                                      (frame-width item))
                             (point-v (view-size item)))))


;; ----------------------------------------------------------------------
;; 3D-title-box-dialog-item
;;	This is copied from UI-Lib (I), and will change.


(defclass 3D-title-box-dialog-item (3D-frame-mixin graphic-item-mixin dialog-item)
  ((title-box-width :initform 0 :accessor title-box-width)
   (frame-color :initarg :frame-color :initform *dark-gray-color*
                :accessor frame-color)
   (frame-pat :initarg :frame-pat :initform *black-pattern*
              :accessor frame-pat)
   ; color-p is only here until I can find out where everything using it is.
   (color-p :initarg :color-p :initform nil :accessor color-p)
   (3d-p :initarg :3d-p :initform nil :accessor 3d-p)))

(defmethod install-view-in-window ((item 3D-title-box-dialog-item) dialog)
  (call-next-method)
  (let* ((topleft (view-position item))
         (bottomright (add-points topleft (view-size item))))
    (rlet ((r :rect :topleft topleft
              :bottomright bottomright))
      (rset r :rect.top (- (rref r :rect.top) 8))
      (#_InvalRect :ptr r)))
  (setf (title-box-width item)
        (string-width (dialog-item-text item)
                      (or (view-font item)
                          (view-font dialog)))))

(defmethod set-dialog-item-text :after ((item 3d-title-box-dialog-item) string)
  (let* ((topleft (view-position item))
         (bottomright (add-points topleft (view-size item)))
         (dialog (view-window item)))
    (rlet ((r :rect :topleft topleft
              :bottomright bottomright))
      (rset r :rect.top (- (rref r :rect.top) 8))
      (#_InvalRect :ptr r))
    (setf (title-box-width item)
          (string-width string
                        (or (view-font item)
                            (view-font dialog))))))

(defmethod view-draw-contents :Around ((item 3D-title-box-dialog-item))
  (with-focused-view item
    ; draw the frame
    (cond ((or (color-p item) (3d-p item))
           (let (shadow hilit)
             (if (typep (view-window item) '3d-window-mixin)
               (setf shadow (shadow-color (view-window item))
                     hilit (hilit-color (view-window item)))
               (setf shadow $chiseling-gray
                     hilit *white-color*))
             (with-fore-color shadow
               (ccl::frame-rect item #@(0 0)
                                (subtract-points (view-size item) #@(1 1))))
             (with-fore-color hilit
               (ccl::frame-rect item #@(1 1) (view-size item)))))
          (t
           (with-pen-saved
             (#_PenPat (or (frame-pat item) *black-pattern*))
             (with-fore-color (or (part-color item :frame) (frame-color item))
               (ccl::frame-rect item #@(0 0) (view-size item)))))))
  ; erase the area around the title and draw the title
  (with-focused-view (view-container item)
    (let* ((topleft (view-position item))
           (bottomright (add-points topleft (view-size item))))
      (with-pstrs ((p-title (dialog-item-text item)))
        (rlet ((r :rect :topleft topleft
                  :bottomright bottomright))
          (rset r rect.left (+ (rref r rect.left) 4))
          (rset r rect.bottom (+ (rref r rect.top) 2))
          (rset r rect.right (+ (rref r rect.left)
                                4
                                (title-box-width item)))
          (#_EraseRect :ptr r))
        (#_MoveTo :long (add-points topleft #@(6 5)))
        (with-fore-color (or (part-color item :text) 0)
          (#_DrawString :ptr p-title))))))
