;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	extensions.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;	Macros and functions adding functionality relating to views.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 4/9/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; view point macros

(defmacro view-size-h (view)
  "Get the horizontal size of view."
  `(point-h (view-size ,view)))

(defmacro view-size-v (view)
  "Get the vertical size of view."
  `(point-v (view-size ,view)))

(defmacro view-position-h (view)
  "Get the horizontal position of view."
  `(point-h (view-position ,view)))

(defmacro view-position-v (view)
  "Get the vertical position of view."
  `(point-v (view-position ,view)))

(defmacro set-view-size-h (view h)
  "Set the horizontal size of view."
  `(set-view-size ,view ,h (view-size-v ,view)))

(defmacro set-view-size-v (view v)
  "Set the vertical size of view."
  `(set-view-size ,view (view-size-h ,view) ,v))

(defmacro set-view-position-h (view h)
  "Set the horizontal position of view."
  `(set-view-position ,view ,h (view-position-v ,view)))

(defmacro set-view-position-v (view v)
  "Set the vertical position of view."
  `(set-view-position ,view (view-position-h ,view) ,v))

(defun upper-left (point-1 point-2)
  "Returns the upper left hand point of the rectangle defined by
the points point-1 and point-2"
  (let ((x1 (point-h point-1)) (y1 (point-v point-1))
        (x2 (point-h point-2)) (y2 (point-v point-2)))
    (make-point (if (< x1 x2) x1 x2)
                (if (< y1 y2) y1 y2))))

(defun lower-right (point-1 point-2)
  "Returns the bottom right hand point of the rectangle defined by
the points point-1 and point-2"
  (let ((x1 (point-h point-1)) (y1 (point-v point-1))
        (x2 (point-h point-2)) (y2 (point-v point-2)))
    (make-point (if (> x1 x2) x1 x2)
                (if (> y1 y2) y1 y2))))

(defmethod view-bottom-right ((self simple-view))
  (let ((dp (view-position self))
        (s  (view-size self)))
    (make-point (+ (point-h dp) (point-h s))
                (+ (point-v dp) (point-v s)))))

(defmethod view-bottom-right ((self view))
  (view-size self))

(defmacro with-view-rect ((name view) &body body)
  (let ((topleft (gensym))
        (bottomright (gensym)))
    `(multiple-value-bind (,topleft ,bottomright)
                          (view-corners ,view)
       (rlet ((,name :rect
                     :topleft ,topleft
                     :bottomright ,bottomright))
         ,@body))))

(defun subviews-in-rect (view top-left bottom-right)
  "Takes a view and the topleft and bottomright coordinates
of a rectagle and returns a list of the subviews whose
view-position is within that rectangle."
  (let ((rect (make-record :rect))
        (views nil))
    (points-to-rect top-left bottom-right rect)
    (do-subviews (subview view)
      (when (point-in-rect-p rect (view-position subview))
        (push subview views)))
    views))

(defun views-right-to-left (views)
  (sort views #'< :key #'(lambda (view) (point-h (view-position view)))))

(defun views-left-to-right (views)
  (sort views #'> :key #'(lambda (view) (point-h (view-position view)))))

(defun views-top-to-bottom (views)
  (sort views #'< :key #'(lambda (view) (point-v (view-position view)))))

(defun views-bottom-to-top (views)
  (sort views #'> :key #'(lambda (view) (point-v (view-position view)))))

(defmacro right-most-view (views)
  `(car (views-right-to-left ,views)))

(defmacro left-most-view (views)
  `(car (views-left-to-right ,views)))

(defmacro top-most-view (views)
  `(car (views-top-to-bottom ,views)))

(defmacro bottom-most-view (views)
  `(car (views-bottom-to-top ,views)))
