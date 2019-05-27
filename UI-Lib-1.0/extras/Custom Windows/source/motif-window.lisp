
; 10079436 26214 5938332

(defclass motif-window (custom-window) 
  ((title-color :initarg :title-color :initform *black-color* :accessor title-color)
   (hilit-color :initarg :hilit-color :initform (disabled-color *gray-color*)
                :accessor hilit-color)
   (shadow-color :initarg :shadow-color 
                 :initform (hilited-color *dark-gray-color*)
                 :accessor shadow-color)
   (color :initarg :color :initform (hilited-color *gray-color*)
          :accessor color)
   (chiseling-depth :initarg :chiseling-depth :initform 1 :accessor
                    chiseling-depth))
  (:Default-initargs :view-font '("Helvetica" 10 :bold)))

; top left bottom right
(defmethod close-box-region ((w motif-window))
  (values 6 6 22 22))

(defmethod zoom-box-region ((w motif-window))
  (values 6 (- (view-size-h w) 22)
          22 (- (view-size-h w) 6)))

(defmethod title-box-region ((w motif-window))
  (values 6 22 22 (- (view-size-h w) 22)))

(defmethod content-region ((w motif-window))
  (values 23 7 (- (view-size-v w) 7) (- (view-size-h w) 7)))

(defmethod title-region ((w motif-window))
  (values 6 22 22 (- (view-size-h w) 22)))

(defmethod resize-box-region ((w motif-window))
  (values (- (the fixnum (view-size-v w)) 16)
          (- (the fixnum (view-size-h w)) 16)
          (- (the fixnum (view-size-v w)) 4)
          (- (the fixnum (view-size-h w)) 4)))

(defmethod draw-title-box ((w motif-window))
  (multiple-value-bind (top left bottom right)
                       (title-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color (color w)
        (#_FillRect r *black-pattern*))
      ; shadow hilit
      (with-3d-colors ((hilit-color w) (shadow-color w))
        (frame-rect-3d r (chiseling-depth w) 
                       (if (point-in-title-box-region-p w (view-mouse-position w))
                         :topleft
                         :bottomright))))
    (with-fore-color (shadow-color w)
      (#_moveto (- left 15) bottom)
      (#_lineto (+ right 15) bottom)
      )))

(defmethod draw-close-box ((w motif-window) &optional selected)
  (declare (ignore-if-unused select))
  (multiple-value-bind (top left bottom right)
                       (close-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color (color w)
        (#_FillRect r *black-pattern*))
      (with-3d-colors ((hilit-color w) (shadow-color w))
        (frame-rect-3d r (chiseling-depth w)
                       (if selected :topleft
                           :bottomright))
        (#_Insetrect r 3 6)
        (with-3d-colors ((hilit-color w) (shadow-color w))
          (frame-rect-3d r 1 (if selected :topleft
                                 :bottomright)))      
        ))))

(defmethod draw-zoom-box ((w motif-window) &optional select)
  (declare (ignore-if-unused select))
  (multiple-value-bind (top left bottom right)
                       (zoom-box-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color (color w)
        (#_FillRect r *black-pattern*))
      (with-3d-colors ((hilit-color w) (shadow-color w))
        (frame-rect-3d r 
                       (chiseling-depth w)
                       (if select :topleft :botright)))      
      (#_Insetrect r 4 4)
      (with-3d-colors ((hilit-color w) (shadow-color w))
        (frame-rect-3d r 1 (if select :topleft :botright)))
      )))

(defmethod draw-title ((w motif-window))
  (with-font-focused-view w
    (with-pstrs ((s (window-title w)))
      (multiple-value-bind (top left bottom right)
                           (title-region w)
        (declare (dynamic-extent top left bottom right))
        (multiple-value-bind (ascent descent max-width leading)
                             (font-info (view-font w))
          (declare (dynamic-extent ascent descent max-width leading)
                   (ignore max-width))
          (let* ((textHeight (+ ascent descent leading))
                 (width (string-width (window-title w) (view-font w)))
                 (h (- right left))
                 (v (- bottom top))
                 (lft (truncate (/ (- h width) 2)))
                 (tp  (truncate (/ (- v textHeight) 2))))
            (#_MoveTo (+ left lft) (+ tp top ascent))     
            (with-fore-color (or (title-color w) 0)
              (#_DrawString s)))
          )))))

(defmethod draw-content ((w motif-window))
  (multiple-value-bind (top left bottom right)
                       (content-region w)
    (declare (dynamic-extent top left bottom right))
    (rlet ((r :rect))
      (ccl::setup-rect r left top right bottom)
      (with-fore-color $light-gray2 (#_paintrect r))
      (with-standard-colors (frame-rect-3d r 1 :bottomright)))))

(defmethod view-draw-contents ((w motif-window))
  (rlet ((r :rect :topleft 0 :bottomright (view-size w)))
    (with-fore-color (color w)
      (#_fillrect r *black-pattern*))
    ; outer
    (with-3d-colors ((hilit-color w) (shadow-color w))
      (frame-rect-3d r (1+ (chiseling-depth w)) :botright))
    (#_insetrect r 5 5)
    ; inner
    (with-3d-colors ((hilit-color w) (shadow-color w))
      (frame-rect-3d r (chiseling-depth w) :topleft))
    (#_insetrect r 2 2)
    (with-fore-color (or (part-color w :content) *white-color*) ;(hilited-color $window-background-gray)
      (#_fillrect r *black-pattern*)))
  (call-next-method))

(defmethod view-click-event-handler :before ((w motif-window) where)
  (draw-title-box w)
  (draw-title w))

(defmethod view-click-event-handler :after ((w motif-window) where)
  (when (wptr w)
    (draw-title-box w)
    (draw-title w)))

#|

(defmethod window-set-color ((w motif-window) new-color)
  (with-slots (color hilit-color shadow-color) w
    (setf color 	 new-color
          hilit-color  (disabled-color new-color)
          shadow-color (hilited-color new-color)))
  (view-focus-and-draw-contents w))
  
(setf w (make-instance 'motif-window))

(let ((w (front-window :class 'motif-window)))
  (with-slots (color hilit-color shadow-color) w
    (setf color 	 *gray-color*      
          hilit-color  *light-gray-color*
          shadow-color *dark-gray-color*))
  (view-focus-and-draw-contents w))

(let ((w (front-window :class 'motif-window)))
  (with-slots (color hilit-color shadow-color) w
    (setf color 	(hilited-color *gray-color*)      
          hilit-color   (disabled-color *gray-color*)
          shadow-color  (hilited-color *dark-gray-color*)))
  (view-focus-and-draw-contents w))

(declaim (special w))

(window-close w)

(view-focus-and-draw-contents w)
(setf (chiseling-depth w) 1)
(setf (chiseling-depth w) 2)
(setf (chiseling-depth w) 3)
(setf (chiseling-depth w) 4)

(window-set-color w 10625594)
(window-set-color w 6488180)
(window-set-color w 15037696)
(window-set-color w (hilited-color *red-color*))
(window-set-color w *red-color*)
(window-set-color w (user-pick-color))
(window-set-color w (get-common-color :cadmium-deep-red))
|#