;; -*- mode:lisp; package:ui -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file: 	ui-lib-ift.lisp
;; author: 	Adam Alpern <aalpern@hampshire.edu>
;; created: 	7/12/1995
;;
;; Code to let you edit UI-LIB dialog-items with IFT. Requires that
;; Interface Tools be loaded prior to evaluating this file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 7/12/95	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhancments to Basic-Editable-Text-DI

(defmethod ift::object-source-code ((item ccl::basic-editable-text-dialog-item))
  (nconc (call-next-method)
         `(:draw-outline ,(slot-value  item 'ccl::draw-outline))))                         

(defmethod ift::add-editor-items :after ((button ccl::basic-editable-text-dialog-item)
                                         editor)
  (let ((position ift::*editor-items-start-pos*))
    (add-subviews
     editor
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       (add-points position #@(0 150)) #@(140 20)
                       "Set Outline Inset"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-width 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-width)
                               (setf (slot-value button 'ccl::draw-outline) new-width)
                               (view-focus-and-draw-contents button))))
                       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spin-Box-View

(interface-tools::add-editable-dialog-item (make-instance 'ui::spin-box-view
                                             :view-size #@(16 24))
                                           #+new-ift-palette :UI-LIB)

(defmethod ift::make-instance-from-prototype ((proto ui-lib::spin-box-view) pos)
  (make-instance (type-of proto)
    :View-size (view-size proto)
    :view-position pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3D-Dialog-Item

(ift::add-editable-dialog-item (make-instance 'ui::3d-dialog-item
                                 :view-size #@(40 40))
                               #+:new-ift-palette :UI-LIB)

(defmethod ift::object-source-code ((item ui::3d-dialog-item))
  (nconc (call-next-method)
         `(:shadow-position ,(ui::shadow-position item))))

(defmethod ift::add-editor-items :after ((icon ui::3d-dialog-item) editor)
  (let ((position ift::*editor-items-start-pos*))
    (add-subviews
     editor
     (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                       position #@(120 16)
                       "Raised"
                       #'(lambda (item)
                           (when (radio-button-pushed-p item)
                             (setf (ui::shadow-position icon) :botright)
                             (view-focus-and-draw-contents icon))) ; change name?
                       :view-nick-name :quit
                       :RADIO-BUTTON-PUSHED-P nil
                       :RADIO-BUTTON-CLUSTER 1)
     (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                       (add-points position #@(0 25)) #@(120 16)
                       "Lowered"
                       #'(lambda (item)
                           (when (radio-button-pushed-p item)
                             (setf (ui::shadow-position icon) :topleft)
                             (view-focus-and-draw-contents icon)))
                       :radio-button-pushed-p t
                       :RADIO-BUTTON-CLUSTER 1)
     (make-dialog-item 'check-box-dialog-item
                       (add-points position #@(0 50)) #@(155 16) "Frame"
                       #'(lambda (item)
                           (if (check-box-checked-p item)
                             (setf (ui::outer-frame icon) t)
                             (setf (ui::outer-frame icon) nil))
                           (invalidate-view icon))
                       :check-box-checked-p nil)
     (make-dialog-item 'button-dialog-item
                       (add-points position #@(0 75)) #@(155 18) "Set 3D Color"
                       #'(lambda (item)
                           (declare (ignore item))
                           (let* ((bg (user-pick-color :color (ui::bg-color icon)))
                                  (3c (3d-colors bg)))
                             (setf (ui::frame-colors icon) 
                                   (cdr 3c))
                             (set-part-color icon :body bg)
                           (invalidate-view icon)))
                       )
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3D-Popup-Menu

(interface-tools::add-editable-dialog-item (make-instance 'ui::3d-pop-up-menu
                                             :view-size #@(60 16)
                                             :view-font '("Geneva" 9 :plain)
                                             :item-display "item 1")
                                           #+new-ift-palette :UI-LIB)

(defmethod ift::make-instance-from-prototype ((proto ui::3d-pop-up-menu) 
                                              pos)
  (let ((m (make-instance (type-of proto)    
             :view-size (view-size proto)
             :view-position pos)))
    (set-view-font m (view-font proto))
    m))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3d-Button-Dialog-Item

(ift::add-editable-dialog-item (make-instance '3d-button-dialog-item
                                 :dialog-item-text "3D Button"
                                 :view-font '("Geneva" 9 :bold)))

(defmethod interface-tools::object-source-code ((item 3d-button-dialog-item))
  (nconc (call-next-method)
         `(:square-p ,(ui::square item)
           :inset ,(ui::inset item)
           :frame ,(ui::frame item)
           :oval-height ,(ui::oval-height item)
           :oval-width ,(ui::oval-width item)
           :colors ',(ui::colors item)
           :hilited-colors ',(ui::hilited-colors item)
           )))

(defmethod ift::add-editor-items :after ((button 3d-button-dialog-item) editor)
  (let ((checked (ui::square button))
        (position ift::*editor-items-start-pos*)
        (default (default-button-p button)))
    (add-subviews
     editor
     (make-dialog-item 'check-box-dialog-item
                       (add-points position #@(0 75)) #@(116 16) "Default Button"
                       #'(lambda (item)
                           (let ((checked (check-box-checked-p item))
                                 (dialog (view-window button))
                                 (old-editor (ift::get-dialog-item-editor button)))
                             (setf (ift::get-dialog-item-editor button) nil)
                             (if checked
                               (set-default-button dialog button)
                               (set-default-button dialog nil))
                             (setf (ift::get-dialog-item-editor button) old-editor)
                             (invalidate-view button)))
                       :check-box-checked-p default)
     (make-dialog-item 'check-box-dialog-item
                       position #@(155 16) "Square"
                       #'(lambda (item)
                           (if (check-box-checked-p item)
                             (setf (ui::square button) t)
                             (setf (ui::square button) nil))
                           (invalidate-view button))
                       :check-box-checked-p checked)
     (make-dialog-item 'check-box-dialog-item
                       (add-points position #@(0 25)) #@(155 16) "Frame"
                       #'(lambda (item)
                           (if (check-box-checked-p item)
                             (setf (ui::frame button) t)
                             (setf (ui::frame button) nil))
                           (invalidate-view button))
                       :check-box-checked-p (ui::frame button))
     (make-dialog-item 'check-box-dialog-item
                       (add-points position #@(0 50)) #@(155 16) "Inset"
                       #'(lambda (item)
                           (if (check-box-checked-p item)
                             (setf (ui::inset button) t)
                             (setf (ui::inset button) nil))
                           (invalidate-view button))
                       :check-box-checked-p (ui::inset button))
     (make-dialog-item 'check-box-dialog-item
                       (add-points position #@(0 100)) #@(155 16) "Embossed Text"
                       #'(lambda (item)
                           (if (check-box-checked-p item)
                             (setf (ui::embossed button) t)
                             (setf (ui::embossed button) nil))
                           (invalidate-view button))
                       :check-box-checked-p (ui::embossed button))
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       (add-points position #@(0 125)) #@(140 20)
                       "Set Oval Height"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-width 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-width)
                               (setf (ui::oval-height button) new-width)
                               (view-focus-and-draw-contents button))))
                       )
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       (add-points position #@(0 150)) #@(140 20)
                       "Set Oval Width"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-width 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-width)
                               (setf (ui::oval-width button) new-width)
                               (view-focus-and-draw-contents button))))
                       )
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       (add-points position #@(0 175)) #@(140 20)
                       "Set Frame Width"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-width 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-width)
                               (setf (ui::frame-width button) new-width)
                               (view-focus-and-draw-contents button))))
                       ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3d-Title-Box-Dialog-Item

(ift::add-editable-dialog-item (make-instance '3d-title-box-dialog-item
                                 :view-size #@(60 40)
                                 :view-font '("Geneva" 9 :plain)
                                 :dialog-item-text "Title Box"
                                 :3d-p nil
                                 :frame-color *dark-gray-color*
                                 :frame-pat *black-pattern*)
                               #+:new-ift-palette :UI-LIB)

(defmethod interface-tools::object-source-code ((item 3d-title-box-dialog-item) )
  (nconc (call-next-method)
         `(:3d-p ,(ui::3d-p item))))

(defmethod ift::add-editor-items :after ((button 3d-title-box-dialog-item) editor)
  (let ((checked (ui::3d-p button))
        (position ift::*editor-items-start-pos*))
    (add-subviews
     editor
     (make-dialog-item 'check-box-dialog-item
                       position #@(155 16) "3D"
                       #'(lambda (item)
                           (if (check-box-checked-p item)
                             (setf (ui::3d-p button) t)
                             (setf (ui::3d-p button) nil))
                           (invalidate-view button))
                       :check-box-checked-p checked)
     
     (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                       (add-points position #@(0 25)) #@(120 16)
                       "Solid Frame"
                       #'(lambda (item)
                           (when (radio-button-pushed-p item)
                             (setf (ui::frame-pat button) *black-pattern*)
                             (view-focus-and-draw-contents button)))
                       :radio-button-pushed-p t
                       :RADIO-BUTTON-CLUSTER 1)
     (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                       (add-points position #@(0 50)) #@(120 16)
                                   "Dotted Frame"
                                   #'(lambda (item)
                                       (when (radio-button-pushed-p item)
                                         (setf (ui::Frame-pat button) *gray-pattern*)
                                         (view-focus-and-draw-contents button)))
                                   :RADIO-BUTTON-CLUSTER 1)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; progress-indicator

(defmethod interface-tools::object-source-code ((item progress-indicator))
  (nconc (call-next-method)
         `(:value ,(ui::value item)
                  :maximum ,(ui::maximum item)
                  :minimum ,(ui::minimum item))))

(interface-tools::add-editable-dialog-item (make-instance 'arc-progress-indicator
                                             :view-size #@(20 20 )
                                             :value 15
                                             )
                                           #+:new-ift-palette :UI-LIB)

(defmethod ift::add-editor-items :after ((item progress-indicator) editor)
  (let ((position ift::*editor-items-start-pos*))
    (add-subviews
     editor
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       position
                       #@(140 20)
                       "Set Minimum"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-min 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-min)
                               (setf (ui::minimum item) new-min)
                               (view-focus-and-draw-contents item))))
                       )
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       (add-points position #@(0 25))
                       #@(140 20)
                       "Set Maximum"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-max 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-max)
                               (setf (ui::maximum item) new-max)
                               (view-focus-and-draw-contents item))))
                       )
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       (add-points position #@(0 50))
                       #@(140 20)
                       "Set Value"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-val 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-val)
                               (setf (ui::value item) new-val)
                               (view-focus-and-draw-contents item))))
                       )
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous

(defmethod ift::add-editor-items :after ((item ui::3d-frame-mixin) editor)
  (let ((position ift::*editor-items-start-pos*))
    (add-subviews
     editor
     (MAKE-DIALOG-ITEM 'button-dialog-item
                       (add-points position #@(0 175)) #@(140 20)
                       "Set Frame Width"
                       #'(lambda (b)
                           (declare (ignore b))
                           (let ((new-width 
                                  (read-from-string
                                   (get-string-from-user "Please enter a number:")
                                   nil nil)))
                             (when (numberp new-width)
                               (setf (ui::frame-width item) new-width)
                               (view-focus-and-draw-contents item))))
                       )
     )))

(ift::add-editable-dialog-item (make-instance '3d-sequence-dialog-item
                                             :view-size #@(20 60)
                                             :table-hscrollp nil
                                             :table-vscrollp nil
                                             :view-font '("Geneva" 9 :plain))
                               #+new-ift-palette :UI-LIB)

(ift::add-editable-dialog-item (make-instance '3d-editable-text-dialog-item
                                             :view-size #@(60 20)
                                             :view-font '("Geneva" 9 :plain)
                                             :dialog-item-text "3D Edit Text")
                                           #+new-ift-palette :UI-LIB)

(ift::add-editable-dialog-item (make-instance '3d-horizontal-line-dialog-item
                                             :view-size #@(80 2))
                                           #+:new-ift-palette :UI-LIB)

;; CCL 3d button
(interface-tools::add-editable-dialog-item (make-instance '3d-button
                                             :frame-p t
                                             :dialog-item-text "3D-button"
                                             :view-font '("Geneva" 9 :bold)))