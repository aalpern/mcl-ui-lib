;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	windoids.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;	Windoid classes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 4/9/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

;----------------------------------------------------------------------
; System 7.5 Windoid - a windoid which uses the System 7.5 windoid
;	WDEF when running under System 7.5 or later.

(defvar *system-7.5-windoid-wdef-handle* nil)

(defun init-windoid-handle ()
  (labels ((system-7.5-check ()
             (let ((version (getf ccl::*environs* :system-version)))
               (when (string-equal version "7.5" :start1 0 :end1 3)
                 (pushnew :system-7.5 *features*))))
           (init-wdef-handle ()
             (setf *system-7.5-windoid-wdef-handle* (#_getresource :|WDEF| 124))))
    (when (system-7.5-check)
      (init-wdef-handle))))

(defclass system-7.5-windoid (windoid) ()
  #+:system-7.5 (:default-initargs
                  :windowdefproc *system-7.5-windoid-wdef-handle*
                  ;; :procid 32
                  ))

(defclass 3d-windoid (3d-window-mixin windoid) ())

(defclass 3d-system-7.5-windoid (3d-window-mixin system-7.5-windoid) ())

#|
(make-instance '3d-windoid)
(make-instance '3d-system-7.5-windoid)
(make-instance '3d-system-7.5-windoid :procid 32)
(make-instance '3d-system-7.5-windoid :procid 31)
|#
