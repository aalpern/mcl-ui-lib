;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file:	gworlds.lisp
;; author:	Adam Alpern
;; created:	9/24/95
;;
;;	Classes, macros, and methods for simplifying offscreen drawing.
;;	To do: do timing tests between GF methods and macros.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 7/19/96	- added to UI-Lib 1.0, put in UI-lib package
;; 9/24/95	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

(export '(
          gworld 				; classes
          gworld-window-mixin
          gworld-view-mixin
          gworld-view
          
          with-locked-pixels 			; state macros
          with-focused-gworld 
          with-bit-blasted-gworld
          
          allocate-gworld			; creation
          make-gworld
          make-gworld-view
          
          portrect portbits pixmap		; accessors
          gworld-portrect gworld-portbits gworld-pixmap
          window-portrect window-portbits window-pixmap
          
          copy-gworld copy-gworld-no-lock	; #_CopyBits wrappers          
          
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [class] : gworld : A class for simplifying dealing with offscreen
;;		      graphics worlds.
;;

(defclass gworld ()
  ((gworld 	:initarg :gworld      :initform nil :accessor gworld-gworld)
   (gdevice 	:initarg :gdevice     :initform (%null-ptr) :accessor gworld-gdevice)
   (depth 	:initarg :depth       :initform nil :accessor gworld-depth)
   (size 	:initarg :size 	      :initform nil :accessor gworld-size)
   (colortable  :initarg :colortable  :initform nil :accessor gworld-colortable)
   (gray-p 	:initarg :gray-p       :initform nil :accessor gworld-gray-p))
  (:default-initargs 
    :depth 8		; 8 bit color
    :size #@(0 0)))

(defmethod default-colortable ((gw gworld))
  (if (gworld-gray-p gw)
    (require-trap #_getctable (case (gworld-depth gw)
                                (2 34) 
                                (4 36) 
                                (8 40)
                                ((0 1 16 32) 8 ; (%null-ptr)
                                 )))
    (require-trap #_getctable (case (gworld-depth gw)
                                (2 2)
                                (4 4)
                                (8 8)
                                ((0 1 16 32) 8 ; (%null-ptr)
                                 )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros for dealing with gworlds
;; For most of the following, an arg called gworld should be
;; an instance of the gworld class. 

(defmacro with-locked-pixels (gworld &body body)
  `(unwind-protect
     (if (require-trap #_LockPixels 
                       (require-trap #_GetGWorldPixmap (gworld-gworld ,gworld)))
       (progn ,@body)
       (error "Unable to lock pixels."))
     (require-trap #_UnLockPixels 
                   (require-trap #_GetGWorldPixmap (gworld-gworld ,gworld)))))

(defmacro with-focused-gworld ((gworld &key (erase t)) &body body)
  "Executes body with the offscreen GWorld of gworld as the current
drawing environment. Warning: does NOT call #_LockPixels. Wrap this in 
a call to with-locked-pixels. The :erase keyword controls whether or not 
the GWorld is first cleared with #_EraseRect (default is t)."
  (let ((port 	 (gensym))
        (gdevice (gensym)))
    `(rlet ((,port    :pointer)
            (,gdevice :pointer))
       (unwind-protect
         (progn 
           (require-trap #_GetGWorld ,port ,gdevice)			; save environment
           (require-trap #_SetGworld (gworld-gworld ,gworld) (%null-ptr))	; make gworld current
           (without-interrupts 
            (when ,erase
              (rlet ((r :rect
                        :topLeft #@(0 0)
                        :bottomRight (gworld-size ,gworld)))
                (require-trap #_EraseRect r)))
            (progn ,@body)))
         (require-trap
          #_SetGWorld (%get-ptr ,port) 			; restore prev environment
          (%get-ptr ,gdevice))
         ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-screen-rect ((var) &body body)
  `(rlet ((,var :rect :left 0 :top 0
                :bottom *screen-height* :right *screen-width*))
     ,@body))

(defmacro make-rect (h &optional v)
  "Make a Macintosh Rect. h is assumed to be a fixnum-encoded point
unless v is supplied, in which case h and v should be integers."
  (if (null v)
    `(make-record :Rect :top 0 :left 0 :bottom (point-v ,h) 
                  :right (point-h ,h))
    `(make-record :Rect :top 0 :left 0 :bottom ,v 
                  :right ,h)))

(defun screen-pixel-depth ()
  (with-screen-rect (r)
    (ccl::screen-bits (require-trap #_GetMaxDevice r))))

(defun screen-color-p ()
  (with-screen-rect (r)
    (ccl::screen-color-p (require-trap #_GetMaxDevice r))))

(defun allocate-gworld (size depth colortable gdevice)
  (rlet ((gw :pointer)
         (r :rect
            :topLeft #@(0 0)
            :bottomRight size))
    (let ((result nil))
      (without-interrupts 
       (setq result (require-trap #_NewGWorld gw
                                  depth r
                                  colortable gdevice 0))
       (assert (zerop result) () "Error creating gWorld"))
      (%get-ptr gw))))

(defun make-gworld (size depth 
                           &key (gworld-gray-p nil) 
                           (gdevice (%null-ptr)))
  "Make a gworld to the given specifications. Returns an instance
of the gworld class."
  (let (it)
    (setq it (make-instance 'gWorld :size size
                            :gray-p gworld-gray-p
                            :depth depth
                            :gdevice gdevice))
    (setf (gworld-colortable it) (default-colortable it))
    (setf (gworld-gworld it) 
          (allocate-gworld size depth (gworld-colortable it) gdevice))
    it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods on gworlds and windows

(defmethod portrect ((gWorld gWorld))
  "gets the portrect of a given gWorld"
  (pref (gworld-gWorld gWorld) :GrafPort.portrect))

(defmethod portrect ((window window))
  "gets the portrect of a given window"
  (pref (wptr window) :GrafPort.portrect))

(defmethod portbits ((gWorld gWorld))
  "returns the current portbits..."
  (when (gworld-gworld gworld)
    (pref (gworld-gworld gworld) :GrafPort.portBits)))

(defmethod portbits ((window window))
  "returns the current portbits..."
  (when (wptr window)
    (pref (wptr window) :GrafPort.portBits)))

(defmethod pixmap ((gWorld gWorld))
  "returns the current pixmap..."
  (require-trap #_GetgWorldPixMap (gWorld gWorld)))

(defmethod pixmap ((window window))
  "returns the current pixmap..."
  (rref (wptr window) :cwindowrecord.port.portpixmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro version of above, for speed.

(defmacro gworld-portrect (gworld)
  `(pref (gworld-gworld ,gworld) :GrafPort.portrect))

(defmacro window-portrect (window)
  `(pref (wptr ,window) :GrafPort.portrect))

(defmacro gworld-portbits (gworld)
  `(pref (gworld-gworld ,gworld) :GrafPort.portBits))

(defmacro window-portbits (window)
  `(pref (wptr ,window) :GrafPort.portBits))

(defmacro gworld-pixmap (gworld)
  `(require-trap #_GetGWorldPixMap (gworld-gworld ,gworld)))

(defmacro window-pixmap (window)
  `(rref (wptr ,window) :cwindowrecord.port.portpixmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copying GWorlds. 
;; These methods are what do the bit-blasting to the screen, or to
;; another gworld.

(defgeneric copy-gworld (src dest &key transfer-mode &allow-other-keys))

(defmethod copy-gworld ((src gworld) (dest window) &key (transfer-mode 0))
  "Copy a GWorld to a window."
  (with-locked-pixels src
    (require-trap #_CopyBits (portbits src) (portbits dest)
                  (portrect src) (portrect dest)
                  transfer-mode  (%null-ptr))))

(defmethod copy-gworld ((src gworld) (dest gworld) &key (transfer-mode 0))
  "copy the gWorld to another gworld"
  (with-locked-pixels src
    (with-locked-pixels dest
      (require-trap #_CopyBits (portbits src) (portbits dest)
                    (portrect src) (portrect dest) 
                    transfer-mode (%null-ptr)))))

; these 2 are for when doing multiple compies within a single
; with-locked-pixels.

(defmethod copy-gworld-no-lock ((src gworld) (dest window) &key (transfer-mode 0))
  "Copy a GWorld to a window, without locking the gworld. Shoudl be wrapped in
a with-locked-pixels so the Memory Manager can't move the pixmap while we're
copying from it."
  (require-trap #_CopyBits (portbits src) (portbits dest)
              (portrect src) (portrect dest)
              transfer-mode  (%null-ptr)))

(defmethod copy-gworld-no-lock ((src gworld) (dest gworld) &key (transfer-mode 0))
  "copy the gWorld to another gworld. Does so without locking either gworld."
  (require-trap #_CopyBits (portbits src) (portbits dest)
              (portrect src) (portrect dest) 
              transfer-mode (%null-ptr)))

; These 2 use macros instead of GFs, for speed
; do timings!

(defmacro fast-copy-gworld-to-window-no-lock (src-gworld dest-window 
                                                          &key (transfer-mode 0))
  `(require-trap #_CopyBits (gworld-portbits ,src-gworld) 
                 (window-portbits ,dest-window)
                 (gworld-portrect ,src-gworld) 
                 (window-portrect ,dest-window)
                 ,transfer-mode  (%null-ptr)))

(defmacro fast-copy-gworld-to-gworld-no-lock (src-gworld dest-gworld
                                               &key (transfer-mode 0))
  `(require-trap #_CopyBits (gworld-portbits ,src-gworld) 
               (gworld-portbits ,dest-gworld)
               (gworld-portrect ,src-gworld) 
               (gworld-portrect ,dest-gworld) 
               ,transfer-mode (%null-ptr)))

; supplying the dest-rect is faster
(defmacro fast-copy-gworld-to-view-no-lock (src-gworld 
                                            dest-view 
                                            &key (transfer-mode 0)
                                            (dest-rect nil))
  (if dest-rect
    `(require-trap #_CopyBits (gworld-portbits ,src-gworld) 
                   (window-portbits (view-window ,dest-view))
                   (gworld-portrect ,src-gworld) 
                   ,dest-rect
                   ,transfer-mode  (%null-ptr))   
    (let ((view-rect (gensym)))
      `(rlet ((,view-rect :rect
                          :topLeft (view-position ,dest-view)
                          :bottomRight (add-points (view-position ,dest-view)
                                                   (view-size ,dest-view))))
         (require-trap #_CopyBits (gworld-portbits ,src-gworld) 
                       (window-portbits (view-window ,dest-view))
                       (gworld-portrect ,src-gworld) 
                       ,view-rect
                       ,transfer-mode  (%null-ptr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-bit-blasted-gworld ((gworld &key (erase t)) 
                                   gworld-or-window &body body)
  "Executes body with the offscreen GWorld of gworld as the current
drawing environment and then copies the GWorld onscreen into window
using #_CopyBits."
  `(with-locked-pixels ,gworld
     (with-focused-gworld (,gworld :erase ,erase)
       ,@body)
     (copy-gworld-no-lock ,gworld ,gworld-or-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get rid of the damnable critters

(defmethod dispose-gworld ((gworld gworld))
  (when (gworld-gworld gworld)
    (require-trap #_DisposeGWorld (gworld-gworld gworld))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [class] : gworld-window : a window with a gworld
;;

(defclass gworld-window-mixin () 
  ((gworld :initarg :gworld :initform nil :accessor gworld))
  (:default-initargs :window-show nil))

(defclass gworld-window (gworld-window-mixin window) ()) 

(defmethod window-close :after ((w gworld-window-mixin))
  (when (gworld w)
    (dispose-gworld (gworld w))))

(defmethod initialize-instance :after ((w gworld-window-mixin) &rest args)
  (declare (ignore args))
  (unless (gworld w)
    (setf (gworld w) (make-gworld (view-size w) (screen-pixel-depth)
                                  :gdevice (%null-ptr))))
  (window-select w))

(defmethod view-draw-contents :around ((view gworld-window-mixin))
  (with-locked-pixels (gworld view)
    (with-focused-gworld ((gworld view))
      (call-next-method))
    (fast-copy-gworld-to-window-no-lock (gworld view) view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [class] : gworld-view : a view with a gworld.
;;

(defclass gworld-view-mixin ()
  ((gworld :initarg :gworld :initform nil :accessor gworld)))

(defclass gworld-view (gworld-view-mixin view) ())

(defmethod remove-view-from-window ((view gworld-view-mixin))
  (when (gworld view)
    (dispose-gworld (gworld view)))
  (call-next-method))

(defmethod initialize-instance :after ((w gworld-view-mixin) &rest args)
  (declare (ignore args))
  (unless (gworld w)
    (setf (gworld w)
          (make-gworld (view-size w) 0))))

(defun make-gworld-view (class &rest initargs)
  (let* ((view-class (if class class 'gworld-view))
         (view (apply #'make-instance view-class initargs)))
    (unless (gworld view)
      (setf (gworld view) 
            (make-gworld (view-size view) 
                         0	; use best depth
                         )))
    view))

(defmethod view-draw-contents :around ((view gworld-view-mixin))
  (with-locked-pixels (gworld view)
    (with-focused-gworld ((gworld view))
      (call-next-method))
    (fast-copy-gworld-to-view-no-lock (gworld view) view)))