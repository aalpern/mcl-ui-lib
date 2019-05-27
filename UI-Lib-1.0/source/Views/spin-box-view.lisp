;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - 1 . 0
;;
;; file: 	spin-box-view.lisp
;; author: 	Adam Alpern
;; created: 	6/6/1997
;;
;; 	see examples:spin-box-example.lisp for examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 6/6/1997	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;; spin-box-view						[ class ]

(defclass spin-box-view (view)
  ((value 	:initarg :value 	:initform nil :accessor spin-box-value)
   (incrementer :initarg :incrementer   :initform nil :accessor spin-box-incrementer)
   (decrementer :initarg :decrementer   :initform nil :accessor spin-box-decrementer)
   (tie :initarg :tie 	  		:initform nil :accessor spin-box-tie)
   (tie-setter 	:initarg :tie-setter 	:initform nil :accessor spin-box-tie-setter)
   (tie-getter 	:initarg :tie-getter 	:initform nil :accessor spin-box-tie-getter)
   (%up-button  	:initform nil 	:accessor spin-box-%up-button)
   (%down-button	:initform nil 	:accessor spin-box-%down-button))
  (:documentation
   "A spin-box is a view which holds a value and lets the user increment or
 decrement that value by means of an up and a down arrow button. It
 can also optionally display that item in a text view, or store that
 value in an abritrary object stored in the spin-box-tie slot.
 The spin-box-incrementer and spin-box-decrementer slots hold a function of
 one argument, which increments or decrements the spin-box-value. The argument
 is the spin-box-value. After setting the value in the spin-box, the
 spin-box-tie-setter function is called with 2 args, the tied object and the value,
 update the tied object with the new value."))

(defgeneric spin-box-increment (view &optional count)
  (:documentation "Increment the value of a spin box and update its tie."))
(defgeneric spin-box-decrement (view &optional count)
  (:documentation "Decrement the value of a spin box and update its tie."))
(defgeneric spin-box-update-tie (view)
  (:documentation "Update the spin box tie, which displays the value"))

(defmethod spin-box-increment ((self spin-box-view) &optional (count 1))
  (with-slots (incrementer tie tie-setter value)
              self
    (dotimes (n count)
      (setf value (funcall incrementer value)))
    (spin-box-update-tie self)))

(defmethod spin-box-decrement ((self spin-box-view) &optional (count 1))
  (with-slots (decrementer tie tie-setter value)
              self
    (dotimes (n count)
      (setf value (funcall decrementer value)))
    (spin-box-update-tie self)))

(defmethod spin-box-update-tie ((view spin-box-view))
  (when (spin-box-tie view) (funcall (spin-box-tie-setter view)
                                     (spin-box-tie view)
                                     (spin-box-value view))))

(defmethod initialize-instance :after ((self spin-box-view) &rest args)
  (declare (ignore args))
  (with-slots (%up-button %down-button
                          incrementer decrementer
                          value tie tie-getter tie-setter) self
    (labels ((update-tie ()
               (when tie
                 (funcall tie-setter tie value)))
             (up-button-action (btn)
               (declare (ignore btn))
               (spin-box-increment self))
             (down-button-action (btn)
               (declare (ignore btn))
               (spin-box-decrement self)))
      (setf %up-button (make-instance '3d-button-dialog-item
                         ;;:cicn (get-ui-resource 1001)
                         ;;:cicn-size #@(12 8)
                         :dialog-item-text "+"
                         :view-font (view-font self)
                         :square t :frame t :inset nil :embossed nil
                         :view-position #@(0 0)
                         :view-size (make-point (view-size-h self)
                                                (truncate (/ (view-size-v self) 2)))
                         :dialog-item-action #'up-button-action)
            %down-button (make-instance '3d-button-dialog-item
                           ;;:cicn (get-ui-resource 1000)
                           ;;:cicn-size #@(12 8)
                           :dialog-item-text "-"
                           :view-font (view-font self)
                           :square t :frame t :inset nil :embossed nil
                           :view-position (make-point 0
                                                      (truncate (/ (view-size-v self) 2)))
                           :view-size (make-point (view-size-h self)
                                                  (truncate (/ (view-size-v self) 2)))
                           :dialog-item-action #'down-button-action))
      (add-subviews self %up-button %down-button))))

(defmethod view-draw-contents ((view spin-box-view))
  (with-slots (%up-button %down-button) view
    (view-draw-contents %up-button)
    (view-draw-contents %down-button)))

(defmethod set-view-size :after ((view spin-box-view) h &optional v)
  (declare (ignore h v))
  (with-slots (%up-button %down-button) view
    (set-view-size %up-button (make-point (view-size-h view)
                                          (truncate (/ (view-size-v view) 2))))
    (set-view-position %down-button
                       (make-point 0 (truncate (/ (view-size-v view) 2))))
    (set-view-size %down-button (make-point (view-size-h view)
                                            (truncate (/ (view-size-v view) 2))))))

(defclass number-di-spin-box-view (spin-box-view) ()
  (:default-initargs :value 0
    :tie-setter #'(lambda (self value)
                    (set-dialog-item-text self
                                          (princ-to-string value)))
    :tie-getter #'(lambda (self)
                    (read-from-string (dialog-item-text self)
                                      nil 0))
    :incrementer #'(lambda (value)
                     (1+ value))
    :decrementer #'(lambda (value)
                     (1- value))))

;; end of file spin-box-view.lisp
