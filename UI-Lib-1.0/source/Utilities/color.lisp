;; -*- mode:lisp; package:ui-lib -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	U I - L I B - I I
;;
;; file: 	color.lisp
;; author: 	Adam Alpern
;; created: 	4/9/1996
;;
;;	Color constants and color-manipulation functions for UI-Lib.
;;	hilited-color and disabled-color from Common Music, by Rick Taube
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 4/19/96	- 3D-hilited-colors
;; 4/10/96	- darken and lighten
;; 4/9/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ui-lib)

(export '($light-gray
          $light-gray2
          $light-gray4
          $medium-light-gray
          $medium-gray
          $window-background-gray
          $chiseling-gray
          darken
          lighten
          3d-colors
          3d-hilited-colors
          ))

;; ----------------------------------------------------------------------
;; Color Constants

(defconstant $light-violet  	13421823)	; from Apple Icon Colors
(defconstant $medium-violet 	10066431)	; from Apple Icon Colors
(defconstant $dark-violet   	6447496)
(defconstant $dark-violet2  	3355494)	; from Apple Icon Colors

; This apple color crap will eventually replace the $RGB-XXX colors
; B&W just for completeness

(defconstant $apple-black   (make-color 0   0   0))
(defconstant $apple-white   (make-color 255 255 255))
(defconstant $apple-gray-1  (make-color 238 238 238))
(defconstant $apple-gray-2  (make-color 221 221 221))
(defconstant $apple-gray-3  (make-color 204 204 204))
(defconstant $apple-gray-4  (make-color 187 187 187))
(defconstant $apple-gray-5  (make-color 170 170 170))
(defconstant $apple-gray-6  (make-color 153 153 153))
(defconstant $apple-gray-7  (make-color 136 136 136))
(defconstant $apple-gray-8  (make-color 119 119 119))
(defconstant $apple-gray-9  (make-color 102 102 102))
(defconstant $apple-gray-10 (make-color 85  85  85))
(defconstant $apple-gray-11 (make-color 68  68  68))
(defconstant $apple-gray-12 (make-color 34  34  34))

;; ----------------------------------------------------------------------
;; Gray Scale Constants

(defconstant $RGB-8bit-gray1 61166)		; Light gray
(defconstant $RGB-8bit-gray2 56797)		; Slightly darker gray
(defconstant $RGB-8bit-gray3 52428)		; Slightly darker gray
(defconstant $RGB-8bit-gray4 48059)		; Slightly darker gray
(defconstant $RGB-8bit-gray5 43690)		; Slightly darker gray
(defconstant $RGB-8bit-gray6 34952)		; Slightly darker gray
(defconstant $RGB-8bit-gray7 30583)		; Slightly darker gray
(defconstant $RGB-8bit-gray8 21845)		; Slightly darker gray
(defconstant $RGB-8bit-gray9 17476)		; Slightly darker gray
(defconstant $RGB-8bit-gray10 8738)		; Slightly darker gray
(defconstant $RGB-8bit-gray11 4369)		; Dark gray

; 4 Bit gray shade constants
(defconstant $RGB-4bit-gray1 49152)		; Light gray
(defconstant $RGB-4bit-gray2 32768)		; Medium gray
(defconstant $RGB-4bit-gray3 8192)		; Dark gray

; Some constants for drawing grays
(defconstant $light-gray  (make-color $RGB-8bit-gray1 $RGB-8bit-gray1 $RGB-8bit-gray1))
(defconstant $light-gray2 (make-color $RGB-8bit-gray2 $RGB-8bit-gray2 $RGB-8bit-gray2))
(defconstant $light-gray4 (make-color $RGB-8bit-gray4 $RGB-8bit-gray4 $RGB-8bit-gray4))
(defconstant $medium-light-gray	(make-color $RGB-8bit-gray5 $RGB-8bit-gray5 $RGB-8bit-gray5))
(defconstant $medium-gray (make-color $RGB-8bit-gray6 $RGB-8bit-gray6 $RGB-8bit-gray6))

(defconstant $window-background-gray	$light-gray)
(defconstant $chiseling-gray		$light-gray4)

; fill shadow hilit
(defconstant $ui-default-colors
  (list $light-gray2 $medium-gray *white-color*))
(defconstant $ui-default-hilited-colors
  (list 10921638 6710886 (make-color 240 240 240)))
#|
(defconstant $ui-red-colors
  (list (darken *red-color*) (darken (darken *red-color*)) *red-color*))
(defconstant $ui-red-hilited-colors
  (mapcar 'hilited-color $ui-red-colors))
(defconstant $ui-green-colors
  (list (darken *green-color*) (darken (darken *green-color*)) *green-color*))
(defconstant $ui-green-hilited-colors
  (mapcar 'hilited-color $ui-green-colors))
(defconstant $ui-blue-colors
  (list (darken *light-blue-color*) (darken (darken *light-blue-color*))
        *light-blue-color*))
(defconstant $ui-blue-hilited-colors
  (mapcar 'hilited-color $ui-blue-colors))
|#

(defparameter *ui-lib-colors* $ui-default-colors)
(defparameter *ui-lib-hilited-colors* $ui-default-hilited-colors)

;; ----------------------------------------------------------------------
;; Color Manipulation Functions

(defun hilited-color (color)
  (if (= color 0)
    #xffffff
    (if (= color #xffffff)
      0
      (let* ((new-color #x000000)
             (r (ldb #xff0000 color))
             (g (ldb #xff00 color))
             (b (ldb #xff color))
             (max (max r g b)))
        (if (>= max #x40)
          (loop for bsp in '(#xff0000 #xff00 #xff)
                for c in (list r g b)
                do (setf new-color (dpb (- c (ash c -2))
                                        bsp new-color)))
          (if (> max 0)
            (loop for bsp in '(#xff0000 #xff00 #xff)
                  for c in (list r g b)
                  with comp = (- #xff max)
                  do (setf new-color (dpb (round (* (/ c max) comp))
                                          bsp new-color)))
            (setf new-color #xffffff)))
        ;(format t "~&converted color #x~6,'0x to #x~6,'0x" color new-color)
        new-color))))

(defun disabled-color (color)
  (let* ((new-color #x000000)
         (r (ldb #xff0000 color))
         (g (ldb #xff00 color))
         (b (ldb #xff color))
         (max (max r g b)))
    (loop for bsp in '(#xff0000 #xff00 #xff)
          for c in (list r g b)
          do (setf new-color (dpb (+ c (ash (- max c) -1)) bsp new-color)))
    ;(format t "~&converted color #x~6,'0x to #x~6,'0x" color new-color)
    new-color))

(defmacro darken (color)
  `(hilited-color ,color))

(defmacro lighten (color)
  `(disabled-color ,color))

(defun 3D-colors (fill-color)
  (let* ((shadow (hilited-color fill-color))
         (hilit (disabled-color fill-color)))
    (when (real-color-equal hilit fill-color) (setq hilit *white-color*))
    (list fill-color shadow hilit)))

(defun 3D-hilited-colors (color)
  (let* ((fill (hilited-color color))
         (shadow (hilited-color fill))
         (hilit (disabled-color fill)))
    (if (real-color-equal hilit fill) (setq hilit *white-color*))
    (list fill shadow hilit)))

;; End of file color.lisp
