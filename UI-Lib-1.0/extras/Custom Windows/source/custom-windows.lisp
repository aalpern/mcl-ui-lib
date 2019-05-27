#|
(make-instance 'example-custom-window :view-font '("Geneva" 9)
               :view-size #@(200 100)
               :window-title "Custom Window"
               )
|#

;(require :ui-lib)

(defclass custom-window (window)
  ()
  (:default-initargs :color-p t
    :window-type :single-edge-box))

(defgeneric draw-resize-handle (custom-window))
(defgeneric draw-title (custom-window))
(defgeneric draw-close-box (custom-window &optional select))
(defgeneric draw-zoom-box (custom-window &optional select))
(defgeneric draw-title-box (custom-window))
(defgeneric draw-content (custom-window))

; Default methods
(defmethod draw-resize-handle (custom-window) 
  (declare (ignore custom-window)) ())
(defmethod draw-title (custom-window) 
  (declare (ignore custom-window)) ())
(defmethod draw-close-box (custom-window &optional select) 
  (declare (ignore custom-window select)) ())
(defmethod draw-zoom-box (custom-window &optional select) 
  (declare (ignore custom-window select)) ())
(defmethod draw-title-box (custom-window) 
  (declare (ignore custom-window)) ())
(defmethod draw-content (custom-window) 
  (declare (ignore custom-window)) ())

(defmethod view-draw-contents :around ((w custom-window))
  (erase-rect w #@(0 0) (view-size w))  
  (draw-content w)
  (call-next-method)
  (draw-resize-handle w)
  (draw-title-box w)
  (draw-title w)
  (draw-close-box w nil)
  (draw-zoom-box w nil))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window Areas

(defgeneric title-region 	(custom-window))
(defgeneric title-box-region 	(custom-window))
(defgeneric close-box-region 	(custom-window))
(defgeneric zoom-box-region 	(custom-window))
(defgeneric resize-box-region 	(custom-window))
(defgeneric content-region 	(custom-window))

; Default methods
(defmethod title-region (custom-window) 
  (declare (ignore custom-window)) (values 0 0 0 0))
(defmethod title-box-region (custom-window) 
  (declare (ignore custom-window)) (values 0 0 0 0))
(defmethod close-box-region (custom-window) 
  (declare (ignore custom-window)) (values 0 0 0 0))
(defmethod zoom-box-region (custom-window) 
  (declare (ignore custom-window)) (values 0 0 0 0))
(defmethod resize-box-region (custom-window) 
  (declare (ignore custom-window)) (values 0 0 0 0))
(defmethod content-region (custom-window) 
  (declare (ignore custom-window)) (values 0 0 0 0))

; ???-region methods return top left bottom right

(defmethod point-in-title-box-region-p ((w custom-window) where)
  (multiple-value-bind (top left bottom right)
                       (title-box-region w)
    (declare (dynamic-extent top left bottom right))
    (ccl::with-rectangle-arg (r left top right bottom)
      (point-in-rect-p r where))))

(defmethod point-in-close-box-region-p ((w custom-window) where)
  (multiple-value-bind (top left bottom right)
                       (close-box-region w)
    (declare (dynamic-extent top left bottom right))
    (ccl::with-rectangle-arg (r left top right bottom)
      (point-in-rect-p r where))))

(defmethod point-in-zoom-box-region-p ((w custom-window) where)
  (multiple-value-bind (top left bottom right)
                       (zoom-box-region w)
    (declare (dynamic-extent top left bottom right))
    (ccl::with-rectangle-arg (r left top right bottom)
      (point-in-rect-p r where))))

(defmethod point-in-resize-box-region-p ((w custom-window) where)
  (multiple-value-bind (top left bottom right)
                       (resize-box-region w)
    (declare (dynamic-extent top left bottom right))
    (ccl::with-rectangle-arg (r left top right bottom)
      (point-in-rect-p r where))))

(defmethod point-in-content-region-p ((w custom-window) where)
  (multiple-value-bind (top left bottom right)
                       (title-box-region w)
    (multiple-value-bind (ctop cleft cbottom cright)
                         (close-box-region w)
      (declare (dynamic-extent top left bottom right))
      (ccl::with-rectangle-arg (r left top right bottom)
        (ccl::with-rectangle-arg (cr cleft ctop cright cbottom)
          (and (point-in-rect-p r where)
               (not (point-in-rect-p cr where))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod view-click-event-handler ((w custom-window) where)
  (cond  ((point-in-close-box-region-p w where)
          (let* ((oldpos (view-mouse-position w))
                 (pos oldpos)
                 (first t))
            (loop while (mouse-down-p)
                  always (progn 
                           (setf oldpos pos)
                           (setf pos (view-mouse-position w)))
                  do
                  (cond ((or (and (point-in-close-box-region-p w pos)
                                  (not (point-in-close-box-region-p w oldpos)))
                             first)
                         (draw-close-box w t))
                        ((or first (and (point-in-close-box-region-p w oldpos)
                                        (not (point-in-close-box-region-p w pos))))
                         (draw-close-box w nil)))
                  (setf first nil))
            (draw-close-box w nil)
            (if (point-in-close-box-region-p w (view-mouse-position w))
              (window-close w))))
         ((point-in-zoom-box-region-p w where)
          (let* ((oldpos (view-mouse-position w))
                 (pos oldpos)
                 (first t))
            (loop while (mouse-down-p)
                  always (progn 
                           (setf oldpos pos)
                           (setf pos (view-mouse-position w)))
                  do
                  (cond ((or (and (point-in-zoom-box-region-p w pos)
                                  (not (point-in-zoom-box-region-p w oldpos)))
                             first)
                         (draw-zoom-box w t))
                        ((or first (and (point-in-zoom-box-region-p w oldpos)
                                        (not (point-in-zoom-box-region-p w pos))))
                         (draw-zoom-box w nil)))
                  (setf first nil))
            (draw-zoom-box w nil)
            (if (point-in-zoom-box-region-p w (view-mouse-position w))
              (window-zoom-event-handler w 8))))
         ((point-in-title-box-region-p w where)
          (window-drag-event-handler w (local-to-global w where)))
         ((point-in-resize-box-region-p w where)
          (window-grow-event-handler w (local-to-global w where))         
          (view-draw-contents w))
         (t
          (call-next-method))))