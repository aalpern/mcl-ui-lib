;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file: 	preferences-dialog.lisp
;; author: 	Adam Alpern
;; created: 	3/12/1996
;;
;;	Implements multi-paned preference dialogs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 04/05/96	- ui-lib conditionals
;;		- disable default button if sheet has no default-function
;; 03/12/96	- rewrote w/ preference-sheets
;; 02/19/96	- file created
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preference sheets
;;
;;	:name
;;		The name of the sheet which will be displayed
;;		when the sheet is selected. If this is not a string,
;;		it will be printed to a string using princ-to-string.
;;	:default-function
;;		This is a function which will be the action function
;;		of the "Defaults" button, and should set sheet items
;;		to default-values. If this is nil, the sheet will
;;		not have a defaults button.
;;	:set-function
;;		The set-action-function for a sheet will be called for
;;		each sheet when the user clicks the "OK" button in a
;;		preference-dialog. It is called with the sheet object
;;		itself as an argument. Use sheet-view-named to find
;;		named subviews of the sheet.
;;	:update-function
;;
;;	:views
;;		The actual views which will be installed when the
;;		user chooses the sheet.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

(defclass preference-sheet ()
  ((name :initarg :name
         :initform ""
         :accessor name)
   (views :initarg :views
          :initform nil
          :accessor views)
   (set-function :initarg :set-function :initform nil :accessor set-function)
   (default-function :initarg :default-function :initform nil
     :accessor default-function)
   (update-function :initarg :update-function :initform nil
                    :accessor update-function)))

(defmethod initialize-instance :after ((sheet preference-sheet) &rest initargs)
  (declare (ignore initargs))
  (unless (stringp (name sheet))
    (setf (name sheet) (princ-to-string (name sheet))))
  (sheet-update sheet))

(defmethod remove-preference-sheet (w sheet)
  (apply #'remove-subviews w (views sheet)))

(defmethod add-preference-sheet (w sheet)
  (apply #'add-subviews w (views sheet))
  (set-dialog-item-text (view-named :prefs-sheet-name w) (name sheet))
  (if (default-function sheet)
    (dialog-item-enable (view-named :prefs-default-button w))
    (dialog-item-disable (view-named :prefs-default-button w)))
  (setf (current-sheet w) sheet))

(defmethod current-sheet-p (w sheet)
  (string-equal (name (current-sheet w)) (name sheet)))

(defun sheet-view-named (name sheet)
  (find name (views sheet) :key #'view-nick-name))

(defun sheet-update (sheet)
  (when (update-function sheet)
    (funcall (update-function sheet) sheet)))

(defun sheet-set (sheet)
  (when (set-function sheet)
    (funcall (set-function sheet) sheet)))

(defun sheet-set-to-defaults (sheet)
  (when (default-function sheet)
    (funcall (default-function sheet) sheet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preference-sequence-dialog-item
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass preference-sequence-dialog-item (#+ui-lib 3d-sequence-dialog-item
                                           #-ui-lib sequence-dialog-item) ())

(defmethod view-click-event-handler ((self preference-sequence-dialog-item) where)
  (declare (ignore where))
  (call-next-method)
  (when (selected-cells self)
    (let ((sheet (cell-contents self (car (selected-cells self)))))
      (set-preference-sheet (view-window self) sheet))))

(defun set-preference-sheet (w sheet-or-name)
  (when sheet-or-name
    (let ((sheet (if (typep sheet-or-name 'preference-sheet)
                   sheet-or-name
                   (find sheet-or-name (sheets w)
                         :test #'string-equal
                         :key  #'name))))
      (unless (current-sheet-p w sheet)
        (remove-preference-sheet w (current-sheet w))
        (erase-rect w #@(130 2) #@(271 138))
        (add-preference-sheet w sheet)
        (invalidate-corners w #@(130 2) #@(271 138))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preferences dialog
;;
;; Uses the following view-nick-names:  :prefs-ok-button, :prefs-cancel-button,
;; :prefs-default-button, :prefs-sheet-list, :prefs-sheet-name
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass preference-dialog (color-dialog)
  ((sheets :initarg :sheets :initform nil :accessor sheets)
   (current-sheet :initarg :current-sheet :initform nil :accessor current-sheet)
   (ok-text :initarg :ok-text :initform "Set" :accessor ok-text))
  (:default-initargs
    :window-show 	nil
    :window-type 	:double-edge-box
    :view-size 		#@(424 271)
    :close-box-p 	nil
    :content-color 	$window-background-gray))

(defmethod initialize-instance :after ((w preference-dialog) &rest initargs)
  (declare (ignore initargs))
  (with-slots (sheets) w
    (apply #'add-subviews w (preference-dialog-aux-items w))
    #+:interface-designer
    (progn
      (view-put w 'interface-tools::horizontal-guides
                (list 33 258 12))
      (view-put w 'interface-tools::vertical-guides
                (list 156 138 411 13)))
    (when sheets
      (set-table-sequence (view-named :prefs-sheet-list w)
                          (mapcar 'name (sheets w)))
      (add-preference-sheet w (first sheets))
      (cell-select (view-named :prefs-sheet-list w) 0))
    (window-show w)))

(defun preference-do-all-sheet-actions (w)
  (dolist (sheet (sheets w))
    (funcall (set-function sheet) sheet)))

;; pieces

(defun preference-dialog-aux-items (&optional w)
  (declare (ignore w))
  (list (make-dialog-item '3d-horizontal-line-dialog-item #@(138 25) #@(274 1) "" 'nil)
        (make-dialog-item 'static-text-dialog-item
                          #@(140 11)  #@(90 13)
                          ""  'nil
                          :view-nick-name :prefs-sheet-name
                          :view-font '("Geneva" 9 :srcor :bold (:color-index 0)))
        (make-dialog-item 'static-text-dialog-item
                          #@(13 11) #@(70 13)
                          "Preferences"
                          'nil
                          :view-font '("Geneva" 9 :srcor :bold (:color-index 0)))
        (make-dialog-item 'preference-sequence-dialog-item
                          #@(14 26)  #@(109 232)
                          "" 'nil
                          :view-font '("Geneva" 9 :srcor :plain (:color-index 0))
                          :cell-size #@(94 12)
                          :view-nick-name :prefs-sheet-list
                          :selection-type :single
                          :table-hscrollp nil
                          :table-vscrollp t
                          :table-sequence nil)
        (make-dialog-item 'button-dialog-item
                          #@(138 239) #@(66 20)
                          "Default"
                          #'(lambda (item)
                              (let ((sheet (current-sheet (view-window item))))
                                (sheet-set-to-defaults sheet)))
                          :view-nick-name :prefs-default-button
                          :default-button nil)
        (make-dialog-item 'button-dialog-item
                          #@(268 239) #@(66 20)
                          "Cancel" #'(lambda (item)
                                       (declare (ignore item))
                                       (return-from-modal-dialog nil))
                          :default-button  nil
                          :view-nick-name :prefs-cancel-button)
        (make-dialog-item 'button-dialog-item
                          #@(346 239) #@(66 20)
                          "Set"
                          #'(lambda (item)
                              (let ((w (view-window item)))
                                (return-from-modal-dialog
                                 (mapcar 'sheet-set (sheets w)))))
                          :default-button t
                          :view-nick-name :prefs-ok-button)))

#|
;EXAMPLE
;-------
; Basically, for each sheet you want to provide 5 things
;	1) The name
;	2) the set-function, for setting the prefs the user has chosen
;	3) the update-function, for displaying the current settings to
;	   the user.
;	4) the default-function, for setting the dialog items in the sheet
;	   to some programmer-specified defaults.
;	5) the subviews

;
; why doesn't MCL already have this?

(defun set-check-box-check (check-box-dialog-item check)
  (if check
    (check-box-check check-box-dialog-item)
    (check-box-uncheck check-box-dialog-item)))

(defparameter *a-foo* t)
(defparameter *a-bar* nil)
(defparameter *a-baz* t)
(defparameter *b-foo* nil)
(defparameter *b-bar* t)
(defparameter *b-baz* nil)

(defun a-default-function (sheet)
  (set-check-box-check (sheet-view-named 'foo sheet) t)
  (set-check-box-check (sheet-view-named 'bar sheet) nil)
  (set-check-box-check (sheet-view-named 'baz sheet) t))

(defun b-default-function (sheet)
  (set-check-box-check (sheet-view-named 'foo sheet) nil)
  (set-check-box-check (sheet-view-named 'bar sheet) t)
  (set-check-box-check (sheet-view-named 'baz sheet) nil))

(defun a-update-function (sheet)
  (set-check-box-check (sheet-view-named 'foo sheet) *a-foo*)
  (set-check-box-check (sheet-view-named 'bar sheet) *a-bar*)
  (set-check-box-check (sheet-view-named 'baz sheet) *a-baz*))

(defun b-update-function (sheet)
  (set-check-box-check (sheet-view-named 'foo sheet) *b-foo*)
  (set-check-box-check (sheet-view-named 'bar sheet) *b-bar*)
  (set-check-box-check (sheet-view-named 'baz sheet) *b-baz*))

(defun a-set-function (sheet)
  (setf *a-foo* (check-box-checked-p (sheet-view-named 'foo sheet)))
  (setf *a-bar* (check-box-checked-p (sheet-view-named 'bar sheet)))
  (setf *a-baz* (check-box-checked-p (sheet-view-named 'baz sheet))))

(defun b-set-function (sheet)
  (setf *b-foo* (check-box-checked-p (sheet-view-named 'foo sheet)))
  (setf *b-bar* (check-box-checked-p (sheet-view-named 'bar sheet)))
  (setf *b-baz* (check-box-checked-p (sheet-view-named 'baz sheet))))

(defun a-pref-sheet ()
  (make-instance 'preference-sheet
    :name "A-Prefs"
    :set-function 	'a-set-function
    :default-function 	'a-default-function
    :update-function 	'a-update-function
    :views (list
            (make-dialog-item 'check-box-dialog-item
                              #@(152 55) #@(137 16)
                              "Foo" 'nil
                              :check-box-checked-p *a-foo*
                              :view-nick-name 'foo)
            (make-dialog-item 'check-box-dialog-item
                              #@(152 78) #@(137 16)
                              "Bar" 'nil
                              :check-box-checked-p *a-bar*
                              :view-nick-name 'bar)
            (make-dialog-item 'check-box-dialog-item
                              #@(152 101) #@(137 16)
                              "Baz" 'nil
                              :check-box-checked-p *a-baz*
                              :view-nick-name 'baz))))

(defun b-pref-sheet ()
  (make-instance 'preference-sheet
    :name "B-Prefs"
    :set-function 	'b-set-function
    :default-function 	'b-default-function
    :update-function 	'b-update-function
    :views (list
            (make-dialog-item 'check-box-dialog-item
                              #@(152 55) #@(137 16)
                              "Foo" 'nil
                              :check-box-checked-p *b-foo*
                              :view-nick-name 'foo)
            (make-dialog-item 'check-box-dialog-item
                              #@(152 78) #@(137 16)
                              "Bar" 'nil
                              :check-box-checked-p *b-bar*
                              :view-nick-name 'bar)
            (make-dialog-item 'check-box-dialog-item
                              #@(152 101) #@(137 16)
                              "Baz" 'nil
                              :check-box-checked-p *b-baz*
                              :view-nick-name 'baz))))

(make-instance 'preference-dialog
  :sheets (list (a-pref-sheet) (b-pref-sheet)))

|#
