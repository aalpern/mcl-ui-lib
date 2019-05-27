;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; icons

#|
(with-open-resource-file (f (choose-file-dialog))
  (defparameter *close-box-icon* (load-color-icon 128))
  (defparameter *close-box-icon-hilited* (load-color-icon 129))
  (defparameter *resize-box-icon* (load-color-icon 134))
  (defparameter *zoom-box-icon* (load-color-icon 132))
  (defparameter *zoom-box-icon-hilited* (load-color-icon 133))
  )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean-custom-window

(defclass clean-custom-window (custom-window)
  ((title-font :initarg :title-font :initform '("Geneva" 9 :bold)
               :accessor title-font)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regions

(defmethod close-box-region ((w clean-custom-window))
  (values 3 3 13 13))

(defmethod zoom-box-region ((w clean-custom-window))
  (values 3 (- (view-size-h w) 13) 13 (- (view-size-h w) 3)))

(defmethod title-box-region ((w clean-custom-window))
  (values 0 0 15 (view-size-h w)))

(defmethod resize-box-region ((w clean-custom-window))
  (values (- (view-size-v w) 13)
          (- (view-size-h w) 13)
          (- (view-size-v w) 3)
          (- (view-size-h w) 3)))

(defmethod content-region ((w clean-custom-window))
  (values 16 0 (view-size-v w) (view-size-h w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw methods

(defmethod draw-content ((w clean-custom-window))
  (multiple-value-bind (top left bottom right)
                       (content-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color $window-background-gray
        (#_paintrect r))
      (with-standard-colors
        (frame-rect-3d r 1 :bottomright))
      )))

(defmethod draw-close-box ((w clean-custom-window) &optional select)
  (multiple-value-bind (top left bottom right)
                       (close-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (#_plotcicon r (if select *close-box-icon-hilited* *close-box-icon*))
      )))

(defmethod draw-zoom-box ((w clean-custom-window) &optional select)
  (multiple-value-bind (top left bottom right)
                       (zoom-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (#_plotcicon r (if select *zoom-box-icon-hilited* *zoom-box-icon*))
      )))

(defmethod draw-title-box ((w clean-custom-window))
  (declare (ignore front))
  (multiple-value-bind (top left bottom right)
                       (title-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      ;; -- Frame
      (with-fore-color $light-gray2
        (#_FillRect r *black-pattern*))
      (with-standard-colors
        (frame-rect-3d r 1 :bottomright))
      (with-fore-color *black-color*
        (#_moveto 0 bottom)
        (#_lineto right bottom))
      ;; -- Horiz. lines
      (with-3d-colors ( $chiseling-gray *white-color* )
        (draw-line-3d #@(16 5) (make-point (- (view-size-h w) 18) 5))
        (draw-line-3d #@(16 7) (make-point (- (view-size-h w) 18) 7))
        (draw-line-3d #@(16 9) (make-point (- (view-size-h w) 18) 9))
        )
      ;; -- Title
      (with-font-codes (font-codes (title-font w))
        (with-pstrs ((p (window-title w)))
          (rlet ((r :rect))
            ; left top right bottom
            (ccl::setup-rect r 16 1 (+ 20 (#_stringwidth p)) 14)
            (with-back-color $light-gray2
              (#_textbox :ptr (%inc-ptr p 1)
               :long (length (window-title w))
               :ptr r
               :word #$teFlushLeft)))))
      )))

(defmethod draw-resize-handle ((w clean-custom-window))
  (multiple-value-bind (top left bottom right)
                       (resize-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (#_insetrect r 1 1)
      (#_plotcicon r *resize-box-icon*)
      )))

#|
(make-instance 'clean-custom-window :view-size #@(120 80))
(make-instance 'clean-custom-window :view-size #@(120 80)
               :window-type :shadow-edge-box)
(make-instance 'clean-custom-window :view-size #@(120 80)
               :window-title "Clean Custom Window")
(make-instance 'clean-custom-window :view-size #@(120 80)
               :window-title "Clean Custom Window"
               :title-font '("Geneva" 9 :plain))
|#
