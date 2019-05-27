(defun author-info-prefs-sheets ()
  (list
   (make-instance 'preference-sheet
    :name "Author Info"
    :views (list (make-dialog-item
          'static-text-dialog-item
          #@(156 33)
          #@(40 12)
          "Author"
          'nil
          :view-font
          '("Geneva" 9 :srcor :bold (:color-index 0)))
        (make-dialog-item
          'static-text-dialog-item
          #@(156 76)
          #@(53 12)
          "Address"
          'nil
          :view-font
          '("Geneva" 9 :srcor :bold (:color-index 0)))
        (make-dialog-item
          'editable-text-dialog-item
          #@(158 51)
          #@(252 15)
          ""
          'nil
          :view-nick-name
          'author-name
          :view-font
          '("Geneva" 9 :srcor :plain (:color-index 0))
          :draw-outline
          -2
          :allow-returns
          nil)
        (make-dialog-item
          'editable-text-dialog-item
          #@(158 93)
          #@(252 15)
          "author-addres"
          'nil
          :view-font
          '("Monaco" 9 :srcor :plain (:color-index 0))
          :draw-outline
          -2
          :allow-returns
          nil)
        (make-dialog-item
          'editable-text-dialog-item
          #@(158 136)
          #@(252 15)
          "Copyright © 1996 Adam Alpern"
          'nil
          :view-nick-name
          'copyright-string
          :view-font
          '("Monaco" 9 :srcor :plain (:color-index 0))
          :draw-outline
          -2
          :allow-returns
          nil)
        (make-dialog-item
          'static-text-dialog-item
          #@(157 119)
          #@(98 12)
          "Copyright String"
          'nil
          :view-font
          '("Geneva" 9 :srcor :bold (:color-index 0)))
        (let ((new
               (make-instance
                 'scrolling-fred-view
                 :view-nick-name
                 'notice
                 :save-buffer-p
                 t
                 :h-scrollp
                 nil
                 :v-scrollp
                 t
                 :wrap-p
                 t
                 :view-size
                 #@(256 50)
                 :view-position
                 #@(156 177))))
          (set-view-font new '("Monaco" 9 :srcor :plain (:color-index 0)))
          new)
        (make-dialog-item
          'static-text-dialog-item
          #@(157 119)
          #@(98 12)
          "Copyright String"
          'nil
          :view-font
          '("Geneva" 9 :srcor :bold (:color-index 0)))
        (make-dialog-item
          'static-text-dialog-item
          #@(156 161)
          #@(98 12)
          "Notice"
          'nil
          :view-font
          '("Geneva" 9 :srcor :bold (:color-index 0)))))))

#|
(make-instance 'preference-dialog
  :sheets (author-info-prefs-sheets))
|#