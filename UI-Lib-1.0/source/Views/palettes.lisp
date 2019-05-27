;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - 1 . 0
;;
;; file: 	palettes.lisp
;; author: 	Adam Alpern
;;
;;	Tool Palettes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 2/10/96	- oops! cicn-3d-button-dialog-item doesn't exist anymore.
;; 10/2/95	- moved into dialog-items.lisp
;; 7/ 6/95 	- removed icon-list.
;;		- removed tool class. Use tool-dialog-item instead.
;;		- oops! all the methods were on tool, not tool-dialog-item.
;;		  fixed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;; thing:	tool-palette
;;		tool-dialog-item
;;
;;		NOTE: view-click-event-handler for tool assumes that the
;;		tool is installed in a tool-palette.
;;

(defclass tool-palette (system-7.5-windoid)
  ((selected-tool :initarg :selected-tool :initform nil
                  :accessor selected-tool)))

(defclass tool-dialog-item (3d-button-dialog-item)
  ((tool-selected-p :initarg :tool-selected-p :initform nil :accessor
                    tool-selected-p)
   (icon-list :initarg :icon-list :initform nil :accessor icon-list)
   (name :initarg :name :initform (gensym "tool-") :accessor name)))

(defmethod hilite-view ((item tool-dialog-item) hilite-flag)
  (when (icon-list item)
    (if hilite-flag
      (setf (cicn item) (second (icon-list item)))
      (setf (cicn item) (first (icon-list item)))))
  (view-draw-contents item))

(defmethod print-object ((tool tool-dialog-item) stream)
  (format stream "#<tool:~a>" (name tool)))

(defmethod tool-select ((tool tool-dialog-item))
  (setf (tool-selected-p tool) 	t)
  (tool-select-handler tool)
  (invalidate-view tool))

(defmethod tool-deselect ((tool tool-dialog-item))
  (tool-deselect-handler tool)
  (setf (tool-selected-p tool) 	nil)
  (setf (hilited tool) nil)
  (invalidate-view tool)
  (view-focus-and-draw-contents tool))

(defmethod view-click-event-handler ((view tool-dialog-item) where)
  (declare (ignore where))
  (switch-tool (view-window view) view))

(defmethod switch-tool ((p Tool-Palette) (new-tool tool-dialog-item))
  (tool-deselect (selected-tool p))
  (tool-select new-tool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric tool-select-handler (tool))
(defgeneric tool-deselect-handler (tool))

(defmethod tool-select-handler ((tool tool-dialog-item))
  (setf (shadow-position tool) :topLeft
        (hilited tool) t))

(defmethod tool-deselect-handler ((tool tool-dialog-item))
  (setf (shadow-position tool) :botRight
        (hilited tool) nil))
