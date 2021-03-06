;; -*- mode:lisp; package:ui -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	resources.lisp
;; author: 	Adam Alpern
;; created: 	4/14/1996
;;
;;	UI-related resource-loading utils.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 4/14/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :resources)

(in-package :ui-lib)

;----------------------------------------------------------------------
; Resource Loading

(defun load-color-icon (id)
  "Load a color icon with the id number id from rsrc-file, which
should be a full pathname to the file. SHould be called within a
with-open-resource-file form if the icon is not in the MCL image."
    (let ((icon-hdl (#_getCicon id)))
      (if (%null-ptr-p icon-hdl)
        (error "no icon resource with id ~s." id)
        icon-hdl)))

(defun load-pixpat (id)
  (let ((ppat-hdl (#_getPixPat id)))
    (if (%null-ptr-p ppat-hdl)
      (error "no ppat resource with id ~s." id)
      ppat-hdl)))

(defun load-and-detach (type id)
  (let* ((res (#_get1resource type id)))
    (#_loadresource res)
    (#_ResError)
    (#_detachresource res)
    (#_HNoPurge res)
    res))

;----------------------------------------------------------------------
; Hash-table resource management
; type id name

(defparameter *ui-resource-spec*
  '(
    ("cicn" 150 "Diagonal Resize Handle") 	; 10x10
    ("cicn" 151 "Small Resize Handle")		;  8x8
    ("cicn" 152 "Resize Handle")		; 10x10

    ("cicn" 1000 "Small Triangle Up") 		; 12x8
    ("cicn" 1001 "Small Triangle Down") 	; 12x8

    ("cicn" 8000 "Triangle Up") 		; 16x16
    ("cicn" 8001 "Triangle Up/Hilited")		; 16x16
    ("cicn" 8002 "Triangle Down")		; 16x16
    ("cicn" 8003 "Triangle Down/Hilited")	; 16x16
    ("cicn" 8004 "Triangle Middle/Hilited")	; 16x16
    )
  )

(defparameter *ui-resource-table*
  (make-hash-table :test #'equal))

(defun load-resource-table (spec table &key (force nil))
  (dolist (res spec)
    (destructuring-bind (type id name)
                        res
      (unless (and (gethash name table) (not force))
        (let ((resource (load-ui-resource type id name)))
          (setf (gethash name table) resource)
          (setf (gethash id table)   resource))))))

(defun load-ui-resource (type id name)
  "Load a resource from a resource specification.
The specification is a list of the format (type id name),
where type is the 4-letter resource type specified as
a string."
  (cond ((string-equal type "cicn")
         (load-color-icon id))
        ((string-equal type "ppat")
         (load-pixpat id))
        (t
         (load-and-detach type id))))

;; ((string-equal type "snd ")
;;        (ccl::get-sound name t)))

(defun get-table-resource (name-or-id table)
  (gethash name-or-id table))

(defun print-resource-table (table &optional (stream t))
  (maphash #'(lambda (key value)
               (format stream "~a:	~a~%" key value))
           table))

(defun get-ui-resource (name-or-id)
  (get-table-resource name-or-id *ui-resource-table*))

#|
(with-open-resource-file (f *ui-resource-file*)
  (setf *ui-resource-table*
        (make-hash-table :test #'equal))
  (load-resource-table *ui-resource-spec* *ui-resource-table*))
(print-resource-table *ui-resource-table*)
|#

(defun init ()
  (init-windoid-handle)
  (with-open-resource-file (f ui::*ui-resource-file*)
    (ui::load-resource-table ui::*ui-resource-spec*
                             ui::*ui-resource-table*
                             :force t))
  (init-twist-icons))
