(let* ((progress (make-dialog-item 'arc-progress-indicator
                                   #@(25 24) #@(55 56)
                                   "" 'nil
                                   :view-nick-name :progress
                                   :value 35
                                   :maximum 100
                                   :minimum 0))
       (text (make-dialog-item '3d-editable-text-dialog-item
                               #@(33 98) #@(39 16)
                               "10" 'nil
                               :view-nick-name :text
                               :part-color-list '(:body 16777164)
                               :draw-outline t
                               :allow-returns nil))
       progress-spin text-spin)
  (setf progress-spin (make-instance 'ui::spin-box-view
                        :view-position #@(86 39)
                        :view-size #@(16 24) 
                        :view-nick-name :progress-spin
                        :tie progress
                        :value (ui::value progress)
                        :tie-setter #'(lambda (prg value)
                                        (setf (ui::value prg) value)
                                        (invalidate-view prg))
                        :tie-getter #'(lambda (prg)
                                        (ui::value prg))
                        :incrementer #'(lambda (value)
                                         (1+ value))
                        :decrementer #'(lambda (value)
                                         (1- value))))
  (setf text-spin (make-instance 'ui-lib::spin-box-view
                    :view-position #@(86 94)
                    :view-size #@(16 24)
                    :view-nick-name :text-spin
                    :tie text
                    :value 10
                    :tie-setter #'(lambda (self value)
                                    (set-dialog-item-text self
                                                          (princ-to-string value)))
                    :tie-getter #'(lambda (self)
                                    (read-from-string (dialog-item-text self)
                                                      0 :eof))  
                    :incrementer #'(lambda (value)
                                     (1+ value))
                    :decrementer #'(lambda (value)
                                     (1- value))))
  (make-instance '3d-dialog
    :window-title "Spin Box Example"
    :view-size #@(131 146)
    :view-subviews
    (list (make-dialog-item
           '3d-title-box-dialog-item
           #@(9 10)
           #@(110 122)
           "Spin Box"
           'nil
           :view-font
           '("Geneva" 9 :srcor :bold (:color-index 0))
           :3d-p
           t)
          progress text progress-spin text-spin)))

