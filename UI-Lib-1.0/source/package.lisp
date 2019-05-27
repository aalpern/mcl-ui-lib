;; -*- mode:lisp; package: (UI-LIB (COMMON-LISP CCL)) -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - 1 . 0
;;
;; file: 	package.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;	Package definition for the UI-Lib package, and exports.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 9-MAR-96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage "UI-LIB"
  (:use "COMMON-LISP" "CCL")
  (:nicknames "UI")

  (:export

   ;; ----------------------------------------
   ;; mixins

   ;; drawing-mixins.lisp
   3d-frame-mixin
   3d-field-mixin
   3d-window-mixin
   background-mixin
   cicn-mixin
   cicn
   cicn-size
   cicn-position

   ;; view-mixins.lisp
   button-mixin
   hilite-view

   ;; ----------------------------------------
   ;; utilities

   ;; color.lisp
   $light-gray
   $light-gray2
   $light-gray4
   $medium-light-gray
   $medium-gray
   $window-background-gray
   $chiseling-gray
   hilited-color
   disabled-color
   darken
   lighten
   3d-colors
   3d-hilited-colors

   ;; drawing.lisp
   with-pen-state
   frame-rect-3d
   view-frame-rect-3d
   with-3d-colors
   with-standard-colors
   ;;frame-rect-3d-with-colors		;*
   ;;frame-rect-3d-with-standard-colors	;*

   draw-string
   draw-line
   draw-line-3d
   fast-paint-rect
   draw-centered-text
   view-fill-rect-with-ppat

   ;; extensions.lisp
   view-size-h
   view-size-v
   set-view-size-h
   set-view-size-v
   view-position-h
   view-position-v
   set-view-position-h
   set-view-position-v
   with-view-rect
   views-right-to-left
   views-left-to-right
   views-top-to-bottom
   views-bottom-to-top

   ;; interaction.lisp
   mouse-sensitive-view-mixin
   mouse-enter-action
   mouse-exit-action
   drag-view-position
   drag-view-size
   draw-rubber-band-line
   drag-selection-rect

   ;; resources.lisp
   load-color-icon
   load-and-detach
   load-pixpat
   load-resource-table
   get-table-resource
   print-resource-table
   get-ui-resource
   load-ui-resource

   ;; ----------------------------------------
   ;; views

   ;; 3D-dialog-items.lisp
   3D-pad	  ;*
   3D-dialog-item
   3D-editable-text-dialog-item
   3D-table-dialog-item
   3D-sequence-dialog-item
   3D-button-dialog-item
   3D-line-dialog-item
   3D-horizontal-line-dialog-item
   3D-vertical-line-dialog-item
   3D-title-box-dialog-item
   3D-scroller

   ;; dialog-items.lisp
   progress-indicator
   arc-progress-indicator
   progress-set-value
   window-resize-dialog-item
   cicn-dialog-item
   cicn-button-dialog-item
   icon-list
   color-swatch-mixin
   color-swatch-dialog-item
   color-swatch-picker-dialog-item
   banner-dialog-item
   twist-down-dialog-item
   draw-twist-down-animation
   draw-twist-up-animation
   line-dialog-item
   horizontal-line-dialog-item
   vertical-line-dialog-item

   ;; palettes.lisp
   tool-palette
   tool-dialog-item
   tool-select-handler
   tool-deselect-handler
   tool-selected-p

   ;; views.lisp
   pict-view
   grid-view
   grid-back-color
   grid-fore-color
   grid-x
   grid-y
   3d-grid-view

   ;; spin-box-view.lisp
   spin-box-view
   spin-box-increment
   spin-box-decrement
   spin-box-update-tie
   spin-box-value
   spin-box-tie
   spin-box-tie-getter
   spin-box-tie-setter
   spin-box-incrementer
   spin-box-decrementer

   ;; list-views
   list-view-mixin
   views-after
   view-shrink-to-fit-subviews
   list-view-adjust-siblings-and-parent
   list-view-has-siblings-p
   add-view-to-list
   remove-view-from-list
   list-view-vsize
   view-list
   list-view
   view-fit-subviews-size

   ;; ----------------------------------------
   ;; windows

   ;; windoids.lisp
   system-7.5-windoid
   3d-windoid
   3d-system-7.5-windoid

   ;; windows.lisp
   3D-dialog
   3D-window

   ;; preferences-dialog.lisp
   preference-sheet
   remove-preference-sheet
   add-preference-sheet
   preference-dialog
   sheet-view-named
   sheet-update
   sheet-set
   sheet-set-to-defaults

   ;; END of UI-LIB exports
   )
  )

;; End of file package.lisp
