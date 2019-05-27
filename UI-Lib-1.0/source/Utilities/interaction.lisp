;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	interaction.lisp
;; author: 	Adam Alpern
;; created: 	7/10/1996
;;
;;	Functions for interaction between views and the user - view
;;	dragging, drawing selection rectangles, drawing "rubber band"
;;	lines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 6/19/97	- added mouse-sensitive-view-mixin
;; 7/10/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

(defclass mouse-sensitive-view-mixin ()
  ((mouse-enter-action :initarg :mouse-enter-action
                       :initform nil
                       :accessor mouse-enter-action)
   (mouse-exit-action :initarg :mouse-exit-action
                      :initform nil
                      :accessor mouse-exit-action)))

(defmethod view-mouse-enter-event-handler ((view mouse-sensitive-view-mixin))
  (when (mouse-enter-action view)
    (funcall (mouse-enter-action view) view)))

(defmethod view-mouse-leave-event-handler ((view mouse-sensitive-view-mixin))
  (when (mouse-exit-action view)
    (funcall (mouse-exit-action view) view)))

(defmethod drag-view-position ((self simple-view)
                               (container view) &optional other-views)
  (with-pen-saved
    (ccl::set-pen-mode container :patxor)
    (ccl::set-pen-pattern self *gray-pattern*)
    (let* ((size (view-size self))
           (view-pos (view-position self))
           (p1 (view-mouse-position container))
           (p2 p1)
           (delta #@(0 0)))
      (declare (fixnum p1  p2 view-pos size delta)
               (dynamic-extent p1 p2 size view-pos delta))
      (loop
        (setf p2 (view-mouse-position container))
        (cond ((mouse-down-p)
               (unless (= p1 p2)
                 (frame-rect container (add-points view-pos delta)
                             (add-points (add-points view-pos delta) size))
                 (dolist (v other-views)
                   (frame-rect container (add-points (view-position v) delta)
                               (add-points (add-points (view-position v) delta) (view-size v))))
                 (setf delta (add-points delta (subtract-points p2 p1)))
                 (setf p1 p2)
                 (frame-rect container (add-points view-pos delta)
                             (add-points (add-points view-pos delta) size))
                 (dolist (v other-views)
                   (frame-rect container (add-points (view-position v) delta)
                               (add-points (add-points (view-position v) delta) (view-size v)))))
               )
              (t (return t))))
      (ccl::set-pen-pattern self *black-pattern*)
      (ccl::set-pen-mode container :patCopy)
      (unless (= delta #@(0 0))
        (set-view-position self (add-points (view-position self) delta))
        (dolist (v other-views)
          (set-view-position v (add-points (view-position v) delta)))))))

(defmethod drag-view-size ((self simple-view) (container view))

  (let ((new-rect (drag-rect container (view-position self)
                             (view-bottom-right self))))
    (cond ((empty-rect-p new-rect) nil)
          (t
           (unless (= (view-position self)
                      (rref new-rect :rect.topleft))
             (set-view-position self (rref new-rect :rect.topleft)))
           (set-view-size self
                          (subtract-points
                           (rref new-rect :rect.bottomright)
                           (rref new-rect :rect.topleft)))))
    (dispose-record new-rect))
  (view-draw-contents self))

(defmethod draw-rubber-band-line ((view view) point
                                  &key
                                  (color *gray-color*)
                                  (width 1)
                                  the-container)
  (let ((container (if the-container
                     the-container
                     (view-container view)))
        where old targetpt)
    (setq where (add-points point (view-position view)))
    (setq old where)
    (with-focused-view container
      (with-fore-color color
        (with-pen-saved
          (set-pen-pattern container *black-pattern*)
          (#_pensize width width)
          (#_penmode (ccl::mode-arg :patxor))
          (loop while (mouse-down-p)
                do (unless (equal old (view-mouse-position container))
                     (draw-line where old)
                     (draw-line where (view-mouse-position container))
                     (setq old (view-mouse-position container)))
                finally (progn
                          (draw-line where (view-mouse-position container))
                          (draw-line where where)
                          (setq targetpt (view-mouse-position container)))))))
    (values (find-view-containing-point container targetpt)
            targetpt)))

(defmethod drag-selection-rect ((self view) where &optional (pat *gray-pattern*))
  "Draw a selection rectangle following the mouse (ala finder, et al)
and return the upper-left and lower-right coordinates of the selected
rect as 2 values."
  (declare (ignore where))
  (with-focused-view self
    (with-pen-state (:pnmode (ccl::mode-arg :patXor)
                             :pnpat pat)
      (let* ((click-origin (view-mouse-position self))
             (p1 click-origin) (p2 p1))
        (loop
          (setf p2 (view-mouse-position self))
          (cond ((mouse-down-p)
                 (unless (= p1 p2)
                   (frame-rect self
                               (upper-left click-origin p2)
                               (lower-right click-origin p2))
                   (frame-rect self
                               (upper-left click-origin p1)
                               (lower-right click-origin p1))
                   (setf p1 p2)))
                (t (return t))))
        (pen-normal self)
        (cond ((= p2 click-origin) nil)
              (t (with-fore-color *white-color*
                   (frame-rect self
                               (upper-left click-origin p2)
                               (lower-right click-origin p2)))
                 (view-draw-contents self)
                 (values (upper-left click-origin p2)
                         (lower-right click-origin p2))))))))

(defclass drag-position-mixin () ())

(defmethod view-click-event-handler :after ((view drag-position-mixin)
                                            where)
  (declare (ignore where))
  (drag-view-position view (view-container view)))

(defmethod drag-rect ((self view) &optional (start (view-mouse-position self))
                      (pos (view-mouse-position self)))
  (with-pen-saved
    (ccl::set-pen-mode self :patxor)
    (ccl::set-pen-pattern self *gray-pattern*)
    (let ((rect (make-record :rect))
          p1 p2)
      (setf p1 (view-mouse-position self))
      (setf p2 p1)
      (loop
        (setf p2 (view-mouse-position self))
        (cond ((mouse-down-p)
               (unless (= p1 p2)
                 (points-to-rect start pos rect)
                 (frame-rect self rect)
                 (setf pos (add-points pos (subtract-points p2 p1)))
                 (setf p1 p2)
                 (points-to-rect start pos rect)
                 (frame-rect self rect))
               )
              (t (points-to-rect start pos rect)
                 (frame-rect self rect)
                 (return t))))
      (ccl::set-pen-mode self :patCopy)
      (ccl::set-pen-pattern self *black-pattern*)
      (points-to-rect start pos rect))))
