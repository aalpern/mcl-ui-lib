;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - 1 . 0
;;
;; file: 	3D-pop-up-menu.lisp
;; author: 	Adam Alpern
;; created: 	10/3/1995
;;
;; Please send comments, improvements, or whatever to address
;; If you redistribute this file, please keep this header intact, and
;; please send me any changes. I would like to know if you use this utility,
;; and if you find it useful.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 11/11/95	- added alt-3d-pop-up-menu
;;		- export 3D-POP-UP-MENU and ALT-3D-POP-UP-MENU
;; 10/03/95	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

(export '(3D-POP-UP-MENU))

(defclass 3D-pop-up-menu (pop-up-menu)
  ((draw-style :initarg :draw-style :initform :3D :accessor draw-style)))

(defun draw-3d-popup-menu (menu items)
  (let* (;(pos (view-position menu))
         (text (menu-title menu)) ;(dialog-item-text menu))
         (ti-rect (ccl::pop-up-menu-title-rect menu))
         (no-title (equal text ""))
         (item-display (pop-up-menu-item-display menu))
         (enabled (menu-enabled-p menu))
         (colorp (ccl::color-or-gray-p menu))
         (pull-down-p (ccl::pull-down-menu-p menu)))
    (with-focused-dialog-item (menu)  ; take font from item, draw in containers coords - this is the other thing that dialog item gives us
      (multiple-value-bind (a d w leading)(ccl::view-font-codes-info menu)
        (declare (ignore a))
        (rlet ((a-rect :rect))
          (copy-record (ccl::pop-up-menu-rect menu) :rect a-rect)
          (let ((mi-title (cond ((eq item-display :selection)
                                 (let ((selection (pop-up-menu-default-item menu)))
                                   (cond ((null items) "<No Items>")
                                         ((zerop selection) "<No selection>")
                                         (t (menu-item-title
                                             (nth (- selection 1) items))))))
                                ((stringp item-display)
                                 item-display)
                                (t
                                 (format nil "~a" item-display)))))
            (with-fore-color (if (and (not enabled) colorp)
                               *gray-color*
                               (part-color menu :menu-title)) ; 21-Jun-91 -wkf
              (with-back-color (part-color menu :menu-body) ; 10-Nov-92 -straz
                (unless no-title
                  (require-trap #_EraseRect :ptr ti-rect)
                  (require-trap #_MoveTo :word (+ (rref ti-rect rect.left) 3) ; (+ (point-h pos) 3)
                                :word (- (rref ti-rect rect.bottom) (+ d leading)))
                  (with-pstrs ((di-title text))
                    (require-trap #_DrawString :ptr di-title)))
                ;  (require-trap #_OffsetRect :ptr a-rect :long #@(0 -1))
                (with-fore-color $light-gray2
                  (require-trap #_FillRect :ptr a-rect :ptr *black-pattern*))
                (cond ((not pull-down-p)
                       (require-trap #_FrameRect :ptr a-rect)
                       (require-trap #_insetrect a-rect 1 1)
                       (frame-rect-3d-with-standard-colors a-rect 1 :botright)
                       (require-trap #_insetrect a-rect -1 -1)
                       (require-trap #_MoveTo :word (+ (rref a-rect rect.left) 3)
                                     :word (rref a-rect rect.bottom))
                       (require-trap #_LineTo :word (rref a-rect rect.right)
                                     :word (rref a-rect rect.bottom))
                       (require-trap #_LineTo :word (rref a-rect rect.right)
                                     :word (+ (rref a-rect rect.top) 2)))
                      ((ccl::crescent menu)
                       (let ((tl (rref a-rect rect.topleft)))
                         (require-trap #_moveto :long tl)
                         (dolist (length '(5 3 2 1 0 0))
                           (require-trap #_line :word length :word 0)
                           (require-trap #_move :word (- length) :word 1)))))
                (require-trap #_InsetRect :ptr a-rect :long #@(1 1))
                (let* ((left (+ (rref a-rect rect.left)(if pull-down-p 6 (max 6 w))))
                       (right (rref a-rect rect.right))
                       (bottom (rref a-rect rect.bottom)))
                  (require-trap #_MoveTo :word left :word  (- bottom (+ leading 1 d)))
                  (with-clip-rect-intersect a-rect
                    (ccl::draw-string-crop mi-title (- right left (if pull-down-p 0 12)))
                    (require-trap #_MoveTo :word (- right (+ 4 11))
                                  :word (- (ash (+ bottom (rref a-rect :rect.top)) -1)
                                           2)))
                  ; Draw the little triangle.
                  (unless pull-down-p
                    (ccl::draw-triangle menu)))))))
        (unless (or enabled colorp)
          (rlet ((ps :penstate))
            (ccl::with-item-rect (rect menu)
              (require-trap #_InsetRect :ptr rect :long #@(0 -1))
              (require-trap #_GetPenState :ptr ps)
              (require-trap #_PenPat :ptr *gray-pattern*)
              (require-trap #_PenMode :word 11)
              (require-trap #_PaintRect :ptr rect)
              (unless no-title (require-trap #_PaintRect ti-rect)) ; ??
              (require-trap #_SetPenState :ptr ps))))))))

(defun alt-draw-3d-popup-menu (menu items)
  (let* (
         (text (menu-title menu)) ;(dialog-item-text menu))
         (ti-rect (ccl::pop-up-menu-title-rect menu))
         (no-title (equal text ""))
         (item-display (pop-up-menu-item-display menu))
         (enabled (menu-enabled-p menu))
         (colorp (ccl::color-or-gray-p menu))
         )
    (with-focused-dialog-item (menu)
      ; take font from item, draw in containers coords -
      ; this is the other thing that dialog item gives us
      (multiple-value-bind (a d w leading)(ccl::view-font-codes-info menu)
        (declare (ignore a))
        (rlet ((a-rect :rect))
          (copy-record (ccl::pop-up-menu-rect menu) :rect a-rect)
          (let ((mi-title (cond ((eq item-display :selection)
                                 (let ((selection (pop-up-menu-default-item menu)))
                                   (cond ((null items) "<No Items>")
                                         ((zerop selection) "<No selection>")
                                         (t (menu-item-title
                                             (nth (- selection 1) items))))))
                                ((stringp item-display)
                                 item-display)
                                (t
                                 (format nil "~a" item-display)))))
            (with-fore-color (if (and (not enabled) colorp)
                               *gray-color*
                               (part-color menu :menu-title)) ; 21-Jun-91 -wkf
              (with-back-color (part-color menu :menu-body) ; 10-Nov-92 -straz

                ;  (require-trap #_OffsetRect :ptr a-rect :long #@(0 -1))
                (with-fore-color $light-gray2
                  (require-trap #_FillRect :ptr a-rect :ptr *black-pattern*))

                (require-trap #_insetrect a-rect 1 1)
                (frame-rect-3d-with-standard-colors a-rect 1 :botright)
                (require-trap #_insetrect a-rect -1 -1)
                (require-trap #_FrameRoundRect a-rect 8 8)

                (let ((height (- (rref a-rect :rect.bottom) (rref a-rect :rect.top))))
                  (require-trap #_moveto (rref a-rect :rect.right) (rref a-rect :rect.top))
                  (require-trap #_move (- (+ 4 height)) 1)
                  (with-fore-color $chiseling-gray
                    (require-trap #_line 0 (- height 2)))
                  (require-trap #_move 1 -1)
                  (with-fore-color *white-color*
                    (require-trap #_line 0 (- (- height 3))))
                  )


                (require-trap #_InsetRect :ptr a-rect :long #@(1 1))
                (let* ((left (+ (rref a-rect rect.left) (max 6 w)))
                       (right (rref a-rect rect.right))
                       (bottom (rref a-rect rect.bottom)))
                  (require-trap #_MoveTo :word left :word  (- bottom (+ leading 1 d)))
                  (with-clip-rect-intersect a-rect
                    (ccl::draw-string-crop mi-title (- right left 20))
                    (require-trap #_MoveTo :word (- right (+ 4 11))
                                  :word (- (ash (+ bottom (rref a-rect :rect.top)) -1)
                                           2)))

                  ; Draw the little triangle.
                  (ccl::draw-triangle menu)

                  )))))

        (unless (or enabled colorp)
          (rlet ((ps :penstate))
            (ccl::with-item-rect (rect menu)
              (require-trap #_InsetRect :ptr rect :long #@(0 -1))
              (require-trap #_GetPenState :ptr ps)
              (require-trap #_PenPat :ptr *gray-pattern*)
              (require-trap #_PenMode :word 11)
              (require-trap #_PaintRect :ptr rect)
              (unless no-title (require-trap #_PaintRect ti-rect)) ; ??
              (require-trap #_SetPenState :ptr ps))))
        ))))

(defmethod view-draw-contents ((menu 3D-pop-up-menu) &aux (items (menu-items menu)))
  (case (draw-style menu)
    (:plain
     (call-next-method))
    (:3D
     (draw-3d-popup-menu menu items))
    (:alt-3D
     (alt-draw-3d-popup-menu menu items))))

#|

(make-instance '3d-window
  :view-size #@(120 240)
  :view-subviews (list
                  (make-instance '3d-title-box-dialog-item
                    :3d-p t
                    :dialog-item-text "Menus"
                    :View-font '("Geneva" 9 :bold)
                    :view-size #@(100 100)
                    :view-position #@(10 10))
                  (make-instance '3d-pop-up-menu
                    :view-position #@(20 20)
                    :view-size #@(80 20)
                    :menu-items
                    (list
                     (make-instance 'menu-item)
                     (make-instance 'menu-item)
                     (make-instance 'menu-item)))
                  (make-instance '3d-pop-up-menu
                    :draw-style :alt-3d
                    :view-position #@(20 50)
                    :view-size #@(80 20)
                    :menu-items
                    (list
                     (make-instance 'menu-item)
                     (make-instance 'menu-item)
                     (make-instance 'menu-item)))
                  (make-instance '3d-pop-up-menu
                    :draw-style :plain
                    :view-position #@(20 80)
                    :view-size #@(80 20)
                    :menu-items
                    (list
                     (make-instance 'menu-item)
                     (make-instance 'menu-item)
                     (make-instance 'menu-item)))

                  ))

|#
