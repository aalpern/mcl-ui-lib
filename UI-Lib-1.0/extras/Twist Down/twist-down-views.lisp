;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	T W I S T - D O W N
;;
;; file: 	twist-down-views.lisp
;; author: 	Adam Alpern <ala@neural.hampshire.edu>
;; created: 	8/9/1995
;;
;; Copyright © 1995 Adam Alpern
;;
;; Please send comments, improvements, or whatever to ala@neural.hampshire.edu.
;; If you redistribute this file, please keep this header intact, and
;; please send me any changes. I would like to know if you use this utility,
;; and if you find it useful.
;;
;;	Views to define a Finder-like 'twist-down' hierarchy of views.
;;	Useful for displaying anything in a tree structure. I use
;;	it for a class browser and a defsystem module/file browser.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Usage
;; -----
;; TWIST-DOWN-VIEW						    [class]
;;	Create subclasses of this class to display your data. The
;;	OBJECT slot contains the data for each node. These are the 
;;	views within a TWIST-DOWN-SCROLLER.
;;
;; TWIST-DOWN-SCROLLER						    [class]
;;	A scrolling view to display twist-down hierarchies in.
;;
;; TWIST-DOWN-HAS-CHILDREN-P view		         [generic function]
;; 	Used to determine if a twist-down view has children, and is
;;	'twist-down-able' or not.
;;
;; TWIST-DOWN-CHILDREN view 			         [generic function]
;;	should return a list of objects which are the children of the
;;	lisp object contained in the OBJECT slot of VIEW. The subviews
;;	to be added when a node is expanded will be created from this list.
;;
;; TWIST-DOWN-MAKE-VIEW view			         [generic function]
;;	specialize this for the kind of object you are displaying. Should
;;	return an instance of twist-down-node-view or a subclass. Don't
;;	worry about setting size, position or depth.
;;
;; TWIST-DOWN-NAME view				         [generic function]
;;	should return a string which will be displayed in the view.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 8/9/1995	- file created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :quickdraw)
(require :scrollers)
(require :resources)

#|
(require :list-views 
         (concatenate 'string 
                      (mac-directory-namestring *loading-file-source-file*)
                      "list-views.lisp"))
|#

(defclass twist-down-view (list-view-mixin view)
  ((object 	:initarg :object 	:initform nil :accessor object)
   (child-views :initarg :child-views 	:initform nil :accessor child-views)
   (name 	:initarg :name 		:initform ""  :accessor name)
   (icon 	:initarg :icon 		:initform nil :accessor icon)
   (depth 	:initarg :depth 	:initform 1   :accessor depth)
   (expanded-p  :initarg :expanded-p 	:initform nil :accessor expanded-p)
   (selected-p  :initarg :selected-p 	:initform nil :accessor selected-p))
  (:default-initargs :vsize 18 :view-font '("Geneva" 9 :plain)))

(defgeneric twist-down-has-children-p (view))
(defgeneric twist-down-children (view))
(defgeneric twist-down-make-view (obj))
(defgeneric twist-down-name (view))

(defun %twist-down-make-subviews (twist-down-view)
  (let* ((children (twist-down-children twist-down-view))
         (views (mapcar #'twist-down-make-view children)))
    (setf (child-views twist-down-view) views)
    views))

(defun %twist-down-install-children-views (twist-down-view)
  (let ((depth (1+ (depth twist-down-view)))
        (subviews (if (child-views twist-down-view)
                    (child-views twist-down-view)
                    (%twist-down-make-subviews twist-down-view))))
    (dolist (v subviews)
      (setf (depth v) depth)
      (add-view-to-list twist-down-view v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Resources

(unless (fboundp 'load-color-icon)
  (defun load-color-icon (id)
    "Load a color icon with the id number id from rsrc-file, which
should be a full pathname to the file. SHould be called within a
with-open-resource-file form if the icon is not in the MCL image."
    (let ((icon-hdl (#_getCicon id)))
      (if (%null-ptr-p icon-hdl)
        (error "no icon resource with id ~s." id)
        icon-hdl))))

(defvar *td-twist-up-icon*		nil)
(defvar *td-twist-up-hilit-icon*	nil)
(defvar *td-twist-down-icon*		nil)
(defvar *td-twist-down-hilit-icon*	nil)
(defvar *td-twist-middle-hilit-icon*	nil)

(with-open-resource-file 
  (f (concatenate 'string 
                  (mac-directory-namestring *loading-file-source-file*)
                  "twist-down.rsrc"))
  (setf *td-twist-up-icon*		(load-color-icon 128))
  (setf *td-twist-up-hilit-icon*	(load-color-icon 129))
  (setf *td-twist-down-icon*		(load-color-icon 130))
  (setf *td-twist-down-hilit-icon*	(load-color-icon 131))
  (setf *td-twist-middle-hilit-icon*	(load-color-icon 132)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ???-region methods return a list of 4 values, the top, left, bottom, and 
;; right coords of the specified region. Also, predicates to see
;; if a point is within a region.

(defmethod twist-region ((view twist-down-view))
  (values 0 0 16 16))

(defmethod icon-region ((view twist-down-view))
  (values 0  (* 16 (the fixnum (depth view)))
          16 (+ 16 (the fixnum (* 16 (the fixnum (depth view)))))))

(defmethod name-region ((view twist-down-view))
  (values 0  (+ 16 (the fixnum (* (the fixnum (depth view)) 16))) 
          16 (point-h (view-size view))))

(defmethod point-in-twist-region-p ((view twist-down-view) where)
  (multiple-value-bind (top left bottom right)
                       (twist-region view)
    (declare (dynamic-extent top left bottom right))
    (ccl::with-rectangle-arg (r left top right bottom)
      (point-in-rect-p r where))))

(defmethod point-in-icon-region-p ((view twist-down-view) where)
  (multiple-value-bind (top left bottom right)
                       (icon-region view)
    (declare (dynamic-extent top left bottom right))
    (ccl::with-rectangle-arg (r left top right bottom)
      (point-in-rect-p r where))))

(defmethod point-in-name-region-p ((view twist-down-view) where)
  (multiple-value-bind (top left bottom right)
                       (name-region view)
    (declare (dynamic-extent top left bottom right))
    (ccl::with-rectangle-arg (r left top right bottom)
      (point-in-rect-p r where))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; view-draw-contents

(defmethod view-draw-contents ((view twist-down-view))
  (declare (optimize (speed 3) (safety 0)))
  (let (top left bottom right)
    (declare (dynamic-extent top left bottom right))
    (with-font-focused-view view
      (rlet ((r :rect :topleft 0 :bottomright (view-size view)))
        (with-fore-color (if (selected-p view)
                           (rgb-to-color (%int-to-ptr #$hilitergb)) 
                           *white-color*)
          (if (window-active-p (view-window view))
            (#_FillRect r *black-pattern*)
            (#_FrameRect r)))
        (when (twist-down-has-children-p view)		;; draw the triangle
          (multiple-value-setq (top left bottom right)
            (twist-region view))
          (ccl::setup-rect r left top right bottom)
          (#_PlotCIcon r (if (expanded-p view) 
                           *td-twist-down-icon* 
                           *td-twist-up-icon*)))
        (when (icon view)				;; draw the icon
          (multiple-value-setq (top left bottom right)
            (icon-region view))
          (ccl::setup-rect r left top right bottom)
          (#_PlotCIcon r (icon view)))
        ;; draw the name
        (#_MoveTo (+ 18 (the fixnum (* (the fixnum (depth view)) 16))) 
         10)
        (with-pstrs ((s (twist-down-name view)))
          (#_DrawString s))
        (call-next-method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; twist-down/up animation drawing

(defmethod draw-twist-down-animation ((view twist-down-view))
  (with-focused-view view
    (multiple-value-bind (top left bottom right)
                         (twist-region view)
      (declare (dynamic-extent top left bottom right))
      (with-back-color (if (selected-p view)
                         (rgb-to-color (%int-to-ptr #$hilitergb))
                         *white-color*)
        (ccl::with-rectangle-arg (r left top right bottom)
          (dolist (icon (list *td-twist-up-hilit-icon*
                              *td-twist-middle-hilit-icon*
                              *td-twist-down-hilit-icon*
                              *td-twist-down-icon*))
            (#_EraseRect r)
            ; kluge - or else it goes by too fast, even in emulation
            ; on a slow powermac
            (dotimes (n 14) (#_PlotCIcon r icon)))
          (#_EraseRect r)))
      )))

(defmethod draw-twist-up-animation ((view twist-down-view))
  (with-focused-view view
    (multiple-value-bind (top left bottom right)
                         (twist-region view)
      (declare (dynamic-extent top left bottom right))
      (with-back-color (if (selected-p view)
                         (rgb-to-color (%int-to-ptr #$hilitergb))
                         *white-color*)
        (ccl::with-rectangle-arg (r left top right bottom)
          (dolist (icon (list *td-twist-down-hilit-icon* 
                              *td-twist-middle-hilit-icon*
                              *td-twist-up-hilit-icon* 
                              *td-twist-up-icon*))
            (#_EraseRect r)
            (dotimes (n 14) (#_PlotCIcon r icon)))
          (#_EraseRect r)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Expanding/Collapsing

(defmethod view-click-event-handler ((view twist-down-view) where)
  (cond ((point-in-twist-region-p view where)
         (expand-or-collapse-twist-down-view view))
        (t
         (view-convert-coordinates-and-click (find-clicked-subview view where)
                                             where view))))

(defmethod expand-or-collapse-twist-down-view ((view twist-down-view))           
  (cond ((and (not (expanded-p view))
              (twist-down-has-children-p view))
         (draw-twist-down-animation view)
         (%twist-down-install-children-views view)
         (setf (expanded-p view) t)
         (invalidate-view view))
        ((expanded-p view)
         (draw-twist-up-animation view)
         (dolist (sv (subviews view))
           #|(when (member sv (selected-views (view-window view)))
             (setf (selected-views (view-window view))
                   (delete sv (selected-views (view-window view)))))|#
           (remove-view-from-list view sv))
         (setf (expanded-p view) nil)
         (invalidate-view view)
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom scroller class for displaying twist-down views

(defclass twist-down-scroller (ccl::scroller) ())

(defmethod view-shrink-to-fit-subviews :after ((view twist-down-view) 
                                               &optional border)
  (declare (ignore border))
  (when (typep (view-container view) 'twist-down-scroller)
    (let ((size (view-fit-subviews-size (view-container view))))
      (setf (slot-value (view-container view) 'ccl::field-size) size)
      (ccl::update-scroll-bars (view-container view) :length (point-v size)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finally, something to throw it all together

(defun make-twist-down-scroller (object &rest
                                            scroller-initargs)
  (let ((scroller (apply #'make-instance 'twist-down-scroller scroller-initargs))
        (td (twist-down-make-view object)))
    (set-view-position td #@(0 0))
    (set-view-size td (make-point (- (point-h (view-size scroller)) 15)
                                  (vsize td)))
    (add-subviews scroller td)
    scroller))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example

(defclass class-node-view (twist-down-view) 
  ()
  (:default-initargs :vsize 15))

(defmethod twist-down-has-children-p ((view class-node-view))
  (class-direct-subclasses (object view)))

(defmethod twist-down-children ((view class-node-view))
  (class-direct-subclasses (object view)))

(defmethod twist-down-make-view ((obj standard-object))
  (make-instance 'class-node-view :object obj))

(defmethod twist-down-name ((view class-node-view))
  (princ-to-string (class-name (object view))))

(make-instance 'color-dialog
  :view-size #@(215 315)
  :view-subviews 
  (list 
   (make-twist-down-scroller 
    (find-class 'standard-object)
    :h-scrollp nil
    :view-position #@(0 0)
    :view-size #@(200 300))))

|#