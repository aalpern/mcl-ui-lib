
(defun shake-window (w &key 
                       (offset #@(4 2))
                       (num 2))
  (let ((old-position (view-position w)))
    (dotimes (n num)
      (set-view-position w (add-points Old-Position offset))
      (set-view-position w Old-Position)
      (set-view-position w (subtract-points Old-Position offset))
      (set-view-position w Old-Position)
      )))

(shake-window (first (windows)))