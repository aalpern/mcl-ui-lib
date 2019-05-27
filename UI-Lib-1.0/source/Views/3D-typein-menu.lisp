(in-package :ui-lib)

(defclass 3d-typein-menu (view) 
  ((menu :initform nil :accessor typein-menu-menu)
   (menu-position :initarg :menu-position :initform :right 
                  :reader typein-menu-menu-position)
   (menu-width :initarg :menu-width :initform nil 
               :reader typein-menu-menu-width)
   (editable-text :initform nil
                  :accessor typein-editable-text)))

(defmethod view-draw-contents ((view 3d-typein-menu))
  (rlet ((r :rect
            :topleft 0
            :bottomright (view-size view)))
    
    ;-- draw frame
    (with-back-color *white-color* (#_eraserect r))
    (frame-rect-3d-with-standard-colors r 1 :topleft)
    (#_insetrect r 1 1)
    (with-fore-color 0 (#_framerect r))
    
    ;-- draw menu square
    (case (typein-menu-menu-position view)
      (:right 	(ccl::setup-rect r 
                                 (- (view-size-h view) (or (typein-menu-menu-width view)
                                                           (view-size-v view)) 1)	; left
                                 1	; top
                                 (- (view-size-h view)  1)	; right
                                 (- (view-size-v view) 1)	; bottom
                                 ))
      (:left 	(ccl::setup-rect r 
                                 1	; left
                                 1	; top
                                 (+ (or (typein-menu-menu-width view)
                                        (view-size-v view))  1)	; right
                                 (- (view-size-v view) 1)	; bottom
                                 )))
    (with-back-color $window-background-gray (#_eraserect r))
    (with-fore-color 0 (#_framerect r))
    (#_insetrect r 1 1)
    (frame-rect-3d-with-standard-colors r 1 :botright)
    
    ;-- draw the triangle
    (typein-menu-menu-width view)
    (#_moveto (case (typein-menu-menu-position view)
                (:right (- (view-size-h view) (truncate (if (typein-menu-menu-width view)
                                                          (typein-menu-menu-width view)
                                                          (view-size-v view))
                                                        2)
                           (if (< 16 (point-v (view-size view)))
                             6 7)))
                (:left 4)) (if (< 16 (point-v (view-size view))) ; big triangle
                              (- (truncate (view-size-v view) 2) 3)
                              (- (truncate (view-size-v view) 2) 2)))
     (draw-triangle view)    
    
    ))

(defmethod draw-triangle ((menu 3d-typein-menu))
  (cond ((< 16 (point-v (view-size menu)))      ; Big triangle
         (#_line :long #@(10 0))
         (#_line :long #@(-5 5))
         (#_line :long #@(-4 -4))
         (#_line :long #@(7 0))
         (#_line :long #@(-3 3))
         (#_line :long #@(-2 -2))
         (#_line :long #@(3 0))
         (#_line :long #@(-1 1)))
        (t
         (#_move 2 0)
         (#_line :long #@(6 0))
         (#_line :long #@(-3 3))
         (#_line :long #@(-2 -2))
         (#_line :long #@(3 0))
         (#_line :long #@(-1 1)))))

#|


(make-instance '3d-dialog
  :view-subviews (list (make-instance '3d-typein-menu
                         :view-position #@(10 10)
                         :view-size #@(200 20)
                         :menu-position :left)
                       (make-instance '3d-typein-menu
                         :view-position #@(10 40)
                         :view-size #@(200 20)
                         :menu-position :right)
                       
                       (make-instance '3d-typein-menu
                         :view-position #@(10 70)
                         :view-size #@(200 20)
                         :menu-position :left
                         :menu-width 16)
                       (make-instance '3d-typein-menu
                         :view-position #@(10 100)
                         :view-size #@(200 20)
                         :menu-position :right
                         :menu-width 16)))
|#