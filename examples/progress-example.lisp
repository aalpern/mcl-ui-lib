(use-package :ui)

(defparameter *foo-files* 
  '(
    "foo.lisp"
    "bar.lisp"
    "baz.lisp"
    "quux.lisp"
    "quuuuuux.lisp"
    "garbly.lisp"
    "imhotep.lisp"
    "quetzalcoatl.lisp"
    "Fneerp!.lisp"
    "urgh.lisp"
    )
  )

(defun foo-views ()
  (list (make-dialog-item 'static-text-dialog-item
                          #@(224 10) #@(46 13)
                          "" 'nil
                          :view-font
                          '("Geneva" 9 :srcor :plain (:color-index 0))
                          :view-nick-name :numleft)
        (make-dialog-item 'static-text-dialog-item
                          #@(69 10) #@(151 16)
                          "Number of files remaining:" 'nil
                          :view-font '("Geneva" 9 :srcor :bold))
        (make-dialog-item 'static-text-dialog-item
                          #@(69 31) #@(62 14)
                          "Compiling:" 'nil
                          :view-font '("Geneva" 9 :srcor :bold))
        (make-dialog-item 'static-text-dialog-item
                          #@(131 31) #@(159 13)
                          "" 'nil
                          :view-nick-name :curfile
                          :view-font '("Geneva" 9 :srcor :italic))))

(let* ((n 1)
       (w (make-instance '3d-dialog
            :window-show nil
            :window-title "Progress Tester"
            :view-size #@(300 64)
            :view-subviews
            (nconc (foo-views)
                   (list (make-instance 'arc-progress-indicator
                    :view-nick-name :progress
                    :view-position #@(5 4)
                    :view-size 	#@(50 50)
                    :maximum (length *foo-files*)
                    :minimum 0
                    :value 0)
                  )))))
  (window-show w)
  (dolist (file *foo-files*)
    (set-dialog-item-text (view-named :curfile w) file)
    (set-dialog-item-text (view-named :numleft w) 
                          (princ-to-string (- (length *foo-files*) n)))
    (progress-set-value (view-named :progress w) n)
    (sleep 1)
    (incf n))
  ;(window-close w)
  )