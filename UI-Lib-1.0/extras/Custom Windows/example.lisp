;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some examples

(use-package :ui)

(declaim (ignore ignore))

(defclass example-custom-window (custom-window) ()
  (:default-initargs :view-font '("geneva" 9 :bold)))   

(defmethod title-region ((w example-custom-window))
  (values 4 16 16 (- (the fixnum (view-size-h w)) 4)))

(defmethod title-box-region ((w example-custom-window))
  (values 3 3 17 (- (the fixnum (view-size-h w)) 3)))

(defmethod zoom-box-region ((w example-custom-window))
  (values 5 (- (the fixnum (view-size-h w)) 15) 
          15 (- (the fixnum (view-size-h w)) 5)))

(defmethod close-box-region ((w example-custom-window))
  (values 5 5 15 15))

(defmethod resize-box-region ((w example-custom-window))
  (values (- (the fixnum (view-size-v w)) 15)
          (- (the fixnum (view-size-h w)) 15)
          (- (the fixnum (view-size-v w)) 5)
          (- (the fixnum (view-size-h w)) 5)))

(defmethod content-region ((w example-custom-window))
  (values 21 4
          (- (the fixnum (view-size-v w)) 4)
          (- (the fixnum (view-size-h w)) 4)))

(defmethod view-draw-contents ((w example-custom-window))
  (rlet ((r :rect :topleft 0 :bottomright (view-size w)))
    (with-fore-color $light-gray4
      (#_paintrect r))
    (with-3d-colors ($window-background-gray
                     (hilited-color $chiseling-gray) 
                     )
      (frame-rect-3d r 1 :botright)))
  (call-next-method))

(defmethod draw-title ((w example-custom-window))
  (with-font-focused-view w
    (with-pstrs ((s (window-title w)))
      (multiple-value-bind (top left bottom right)
                           (title-region w)
        (declare (dynamic-extent top left bottom right))
        (with-fore-color *white-color*
          (rlet ((r :rect))          
            (ccl::setup-rect r left top right bottom)
            ; (#_TextBox s (length (window-title w)) r :left)
            (#_MoveTo 24 14)
            (#_DrawString s)))))))

(defmethod draw-resize-handle ((w example-custom-window))
  (multiple-value-bind (top left bottom right)
                       (resize-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color $window-background-gray
        (#_FillRect r *black-pattern*))
      (with-3d-colors (*white-color* $medium-gray)
        (frame-rect-3d r 1 :topleft)
        ))))

(defmethod draw-close-box ((w example-custom-window) &optional select)
  (multiple-value-bind (top left bottom right)
                       (close-box-region w)  
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color (if select 
                         *gray-color* 
                         $light-gray2 )
        (#_InsetRect r 1 1)
        (#_FillRect r *black-pattern*))
      (#_InsetRect r -1 -1)      
      (with-3d-colors (*white-color* $medium-gray)
        (frame-rect-3d r 1 (if select :topleft :botright)))
      )))

(defmethod draw-zoom-box ((w example-custom-window) &optional select)
  (multiple-value-bind (top left bottom right)
                       (zoom-box-region w)  
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color (if select 
                         *gray-color* 
                         $light-gray2 )
        (#_InsetRect r 1 1)
        (#_FillRect r *black-pattern*))
      (#_InsetRect r -1 -1)      
      (with-3d-colors (*white-color* $medium-gray)
        (frame-rect-3d r 1 (if select :topleft :botright)))
      )))

(defmethod draw-title-box ((w example-custom-window))
  (multiple-value-bind (top left bottom right)
                       (title-box-region w)    
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-3d-colors ($window-background-gray
                       (lighten $chiseling-gray))
        (frame-rect-3d r 1 :topleft))
      (#_insetrect r 1 1)
      (with-fore-color (darken (darken *red-color*))
        (#_FillRect r *black-pattern*)))))

; (make-instance 'example-custom-window)