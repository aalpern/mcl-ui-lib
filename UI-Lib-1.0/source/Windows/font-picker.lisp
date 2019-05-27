;; -*- mode:lisp; package:ui -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file  :	font-picker.lisp
;; author:	Adam Alpern
;; date  :	6/26/95
;;
;; Please send comments, improvements, or whatever to ala@neural.hampshire.edu.
;; If you redistribute this file, please keep this header intact, and
;; please send me any changes. I would like to know if you use this utility,
;; and if you find it useful.
;;
;; 	A simple font-picker dialog. The function (user-pick-font) returns
;; 	a font spec in the style '("Geneva" 9 :plain). Initargs for class
;;	'dialog may be supplied, and will be supplied to make-instance.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 07/12/95	-
;; 07/07/95	- fixed erroneous :condensed, :extended font styles.
;; 06/26/95	- File created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

(export (list 'font-picker-dialog 'user-pick-font))

(defparameter *font-styles* (list :bold :italic :underline :outline :shadow
                                  :condense :extend))

(defclass attribute-menu-item (menu-item)
  ((attribute :initarg :attribute :initform nil :accessor attribute)))

(defun font-menu-items ()
  (mapcar #'(lambda (font-name)
              (make-instance 'attribute-menu-item
                :menu-item-title font-name
                :attribute font-name))
          (remove #\% *font-list*
                  :key #'(lambda (string)
                           (elt string 0)))))

(defun font-size-menu-items ()
  (mapcar #'(lambda (size)
              (make-instance 'attribute-menu-item
                :menu-item-title (princ-to-string size)
                :attribute size))
          (list 9 10 12 14 18 24 36 48 73)))

(defun font-picker-font-pop-up-menu ()
  (list (make-instance 'pop-up-menu
          :view-position #@(58 8)
          :view-size #@(148 20)
                          :view-font '("Geneva" 9 :srcor :plain)
                          :menu-items (font-menu-items)
                          :view-nick-name :font-menu)))

(defun font-picker-font-size-pop-up-menu ()
  (list (make-instance 'pop-up-menu
          :view-position #@(58 39)
          :view-size #@(42 20)
          :view-font '("Geneva" 9 :srcor :plain)
          :view-nick-name :font-size-menu
          :menu-items
          (font-size-menu-items))))

(defun font-picker-text-items ()
        (list (make-dialog-item
         'static-text-dialog-item
         #@(24 11)
         #@(32 16)
         "Font:"
         'nil
         :view-font
         '("Geneva" 9 :srcor :plain))
        (make-dialog-item
         'static-text-dialog-item
         #@(19 69)
         #@(32 16)
         "Style:"
         'nil
         :view-font
         '("Geneva" 9 :srcor :plain))
        (make-dialog-item
         'static-text-dialog-item
         #@(24 40)
         #@(32 16)
         "Size:"
         'nil
         :view-font
         '("Geneva" 9 :srcor :plain))))

(defun font-picker-check-box-items ()
        (list (make-dialog-item 'check-box-dialog-item
                                #@(135 67)
                                #@(72 16)
                                "Outline"
                                'maybe-uncheck-plain-item
                                :view-font 		'("Geneva" 9 :srcor :plain)
                                :check-box-checked-p 	nil
                                :view-nick-name 	:outline)
              (make-dialog-item 'check-box-dialog-item
                                #@(135 85)
                                #@(72 16)
                                "Shadow"
                                'maybe-uncheck-plain-item
                                :view-font
                                '("Geneva" 9 :srcor :plain)
                                :check-box-checked-p
                                nil
                                :view-nick-name :shadow)
              (make-dialog-item
               'check-box-dialog-item
               #@(135 103)
               #@(72 16)
               "Condensed"
               'maybe-uncheck-plain-item
               :view-font
               '("Geneva" 9 :srcor :plain)
               :check-box-checked-p
               nil
               :view-nick-name :condense)
              (make-dialog-item
               'check-box-dialog-item
               #@(135 121)
               #@(72 16)
               "Extended"
               'maybe-uncheck-plain-item
               :view-font
               '("Geneva" 9 :srcor :plain)
               :check-box-checked-p
               nil
               :view-nick-name :extend)
              (make-dialog-item
               'check-box-dialog-item
               #@(57 67)
               #@(72 16)
               "Plain"
               #'(lambda (item)
                   (when (check-box-checked-p item)
                     (dolist (style *font-styles*)
                       (check-box-uncheck (find-named-sibling item style)))))
               :view-font
               '("Geneva" 9 :srcor :plain)
               :check-box-checked-p
               t
               :view-nick-name :plain)
              (make-dialog-item
               'check-box-dialog-item
               #@(57 85)
               #@(72 16)
               "Bold"
               'maybe-uncheck-plain-item
               :view-font
               '("Geneva" 9 :srcor :plain)
               :check-box-checked-p
               nil
               :view-nick-name :bold)
              (make-dialog-item
               'check-box-dialog-item
               #@(57 103)
               #@(72 16)
               "Italic"
               'maybe-uncheck-plain-item
               :view-font
               '("Geneva" 9 :srcor :plain)
               :check-box-checked-p
               nil
               :view-nick-name :italic)
              (make-dialog-item
               'check-box-dialog-item
               #@(57 121)
               #@(72 16)
               "Underline"
               'maybe-uncheck-plain-item
               :view-font
               '("Geneva" 9 :srcor :plain)
               :check-box-checked-p
               nil
               :view-nick-name :underline)))

(defun font-picker-buttons ()
        (list (make-dialog-item
         'button-dialog-item
         #@(116 157)
         #@(62 20)
         "OK"
         'font-picker-ok-action
         :default-button
         t)
        (make-dialog-item
         'button-dialog-item
         #@(38 157)
         #@(62 20)
         "Cancel"
         #'(lambda (item)
             (declare (ignore item))
             (return-from-modal-dialog nil))
         :default-button
         nil
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun font-picker-ok-action (item)
  (let ((font (attribute (selected-item (find-named-sibling item :font-menu))))
        (size (attribute (selected-item (find-named-sibling item :font-size-menu))))
        (style-list (if (check-box-checked-p (find-named-sibling item :plain))
                      (list :plain)
                      (delete nil
                              (mapcar #'(lambda (nickname)
                                          (when (check-box-checked-p
                                                 (find-named-sibling item nickname))
                                            nickname))
                                      *font-styles*)))))
    (return-from-modal-dialog (apply #'list font size style-list))
    ))

(defun maybe-uncheck-plain-item (item)
  (when (check-box-checked-p (find-named-sibling item :plain))
    (check-box-uncheck (find-named-sibling item :plain))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass font-picker-dialog (dialog) ()
  (:default-initargs
    :window-type   	:movable-dialog
    :window-title  	"Font Style"
    :view-size 		#@(220 189)
    :close-box-p 		nil))

(defun font-picker-dialog (&rest initargs)
  (apply #'make-instance 'font-picker-dialog
         :view-subviews
         (append (font-picker-font-pop-up-menu)
                 (font-picker-font-size-pop-up-menu)
                 (font-picker-text-items)
                 (font-picker-check-box-items)
                 (font-picker-buttons))
         initargs))

(defun user-pick-font (&rest initargs)
  (modal-dialog (apply #'font-picker-dialog initargs)))

;; (user-pick-font)
