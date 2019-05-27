;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - 1 . 0
;;
;; file: 	list-views.lisp
;; author: 	Adam Alpern
;; created: 	7/21/1995
;;
;;	Views that arrange and size themselves based on their subviews.
;;	Mostly aping a lot of stuff that the list manager prob'ly does
;;	for me but I'm too lazy to figure out LDEFs. The whole thing was
;;	a late-night hack that needs to be re-written for efficiency as
;;	it's horribly kluged up. Also duplicates some of the functionality
;;	of Richard Lynch's twist-down.lisp, but not as generally useful.
;;	I should clean this up and generalize it.
;;
;; 	pushend, extreme, maximize, minimize, and view-shrink-to-fit-subviews
;; 	were written by Michael Travers, and the following copyright notice applies
;; 	to them:
;; 		Copyright � 1994-95 Michael Travers
;; 		Permission is given to use and modify this code as long
;; 		as the copyright notice is preserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 03/24/2004   - moved into UI-Lib-1.0 project
;; 07/21/1995	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

(declaim (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0))
         (inline views-after list-view-has-siblings-p maximize minimize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Mike Travers
(defmacro pushend (thing place)
  `(setf ,place
         (nconc ,place
                (list ,thing))))

(defun extreme (list test &key (key #'identity) (return-max nil))
  (and list
       (let* ((best (car list))
              (max (funcall key best)))
         (dolist (other (cdr list) (if return-max max best))
           (let ((score (funcall key other)))
	     (when (funcall test score max)
               (setq best other max score)))))))

(defun maximize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'> :key key :return-max return-max))

(defun minimize (list &key (key #'identity) (return-max nil))
  (declare (inline extreme))            ; not that this does anything
  (extreme list #'< :key key :return-max return-max))

(defmethod view-shrink-to-fit-subviews ((view view) &optional (border 10))
  (let* ((subviews (subviews view))
         (max-h (maximize subviews
                          :key #'(lambda (sv) (+ (point-h (view-size sv))
                                                 (point-h (view-position sv))))
                          :return-max t))
         (max-v (maximize subviews
                          :key #'(lambda (sv) (+ (point-v (view-size sv))
                                                 (point-v (view-position sv))))
                          :return-max t)))
    (when subviews
      (set-view-size view (make-point (+ max-h border) (+ max-v border))))))
; end MT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass list-view-mixin ()
  ((vsize :initarg :vsize :initform 18 :accessor list-view-vsize)
   (view-list :initarg :view-list :initform nil :accessor view-list)))

(defmethod set-view-size :after ((view list-view-mixin) h &optional v)
  (declare (ignore h v))
  (when (typep (view-container view) 'list-view-mixin)
    (view-shrink-to-fit-subviews (view-container view) 0)))

(defun views-after (view list-view-mixin)
  (rest (member view (view-list list-view-mixin))))

(defun list-view-has-siblings-p (list-view)
  (and (typep (view-container list-view) 'list-view-mixin)
       (remove list-view (subviews (view-container list-view)))))

(defun list-view-adjust-siblings-and-parent (view)
  (declare (optimize (speed 3) (safety 0)))
  (when (list-view-has-siblings-p view)
    (let ((after-views (views-after view (view-container view)))
          delta)
      (when after-views
        (setq delta (- (+ (the fixnum (point-v (view-position view)))
                          (the fixnum (point-v (view-size view))))
                       (the fixnum (point-v (view-position (car after-views))))))
        (dolist (av after-views)
          (set-view-position av (make-point (point-h (view-position av))
                                            (+ (the fixnum delta)
                                               (the fixnum (point-v (view-position av)))))))))
    (view-shrink-to-fit-subviews (view-container view) 0))
  (when (typep (view-container view) 'list-view-mixin)
   (list-view-adjust-siblings-and-parent (view-container view))))

(defmethod add-view-to-list ((view list-view-mixin) v)
  (declare (optimize (speed 3) (safety 0)))
  (let (lastview vpos)
    (setq lastview (car (last (view-list view))))
    (if lastview
      (setq vpos (+ (1- (the fixnum (point-v (view-position lastview))))
                    (the fixnum (list-view-vsize view))))
      (setq vpos (list-view-vsize view)))
    (pushend v (view-list view))
    (set-view-size v (make-point (the fixnum (point-h (view-size view)))
                                 (the fixnum (point-v (view-size v)))))
    (set-view-position v (make-point 0 vpos))
    (add-subviews view v)
    (view-shrink-to-fit-subviews view 0)
    (list-view-adjust-siblings-and-parent view))
  v)

(defmethod remove-view-from-list ((view list-view-mixin) v)
  (declare (optimize (speed 3) (safety 0)))
  (let ((after-views (views-after v view)))
    (remove-subviews view v)
    (setf (view-list view) (delete v (view-list view)))
    (when (list-view-has-siblings-p v)
      (list-view-adjust-siblings-and-parent v))
    (when after-views
      (dolist (av after-views)
      (set-view-position av
                         (make-point 0
                                     (- (the fixnum (point-v (view-position av)))
                                        (the fixnum (list-view-vsize view)))))
      ))
    (view-shrink-to-fit-subviews view 0)
    (list-view-adjust-siblings-and-parent view)
    (when (null (view-list view))
      (set-view-size view (make-point (point-h (view-size view)) (list-view-vsize view)))
      (list-view-adjust-siblings-and-parent view))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod view-fit-subviews-size ((view view) &optional (border 10))
  (let* ((subviews (subviews view))
         (max-h (ui::maximize subviews
                              :key #'(lambda (sv) (+ (point-h (view-size sv))
                                                     (point-h (view-position sv))))
                              :return-max t))
         (max-v (ui::maximize subviews
                              :key #'(lambda (sv) (+ (point-v (view-size sv))
                                                     (point-v (view-position sv))))
                              :return-max t)))
    (when subviews
      (make-point (+ max-h border) (+ max-v border)))))

(defclass list-view (list-view-mixin view) ())

(defmethod view-shrink-to-fit-subviews :after ((view list-view)
                                               &optional border)
  (declare (ignore border))
  (when (typep (view-container view) 'ccl::scroller)
    (let ((size (view-fit-subviews-size (view-container view))))
      (setf (slot-value (view-container view) 'ccl::field-size) size)
      (ccl::update-scroll-bars (view-container view) :length (point-v size)))))
