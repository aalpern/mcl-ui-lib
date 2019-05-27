;; -*- mode:lisp; syntax:common-lisp; package:ccl -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file    : sound.lisp
;; author  : Adam Alpern (ala@neural.hampshire.edu)
;; created : 03/16/95
;;
;; Copyright © 1995 Adam Alpern
;;
;; Please send comments, improvements, or whatever to ala@neural.hampshire.edu.
;; If you redistribute this file, please keep this header intact, and
;; please send me any changes. I would like to know if you use this utility,
;; and if you find it useful.
;;
;; Revision History
;; ----------------
;; 03/16/95	- File created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; for some reason, trap autoloading doesn't always work on our 8100
; under sys 7.5. This shouldn't be necessary.
; (traps::require-interface 'sound)

(defvar *UI-snd-channel-1* nil)
(defvar *UI-snd-channel-2* nil)
(defvar *UI-snd-channel-3* nil)
(defvar *UI-snd-channel-4* nil)

(defmacro play-snd-sync (sndhndl)
  `(require-trap #_SndPlay (%null-ptr) ,sndhndl t))

(defmacro play-snd-async (sndhndl channel)
  "Play a 'snd ' resource asynchronously on the specified sound channel.
sndchannell should be a sound channel created with #_sndnewchannel. Be
sure to call (ui-init-snd-channels) before using any of the *UI-snd-channel*
variables."
  `(require-trap #_SndPlay ,channel ,sndhndl t))

(defun get-sound (name-string &optional (detach? nil))
  "Get the 'snd ' resource with the specified name. If detach? is t, 
we call #_detachresource on the sound, which we want to do if the sound
is to be used after its resource file is closed. Returns a handle to
the sounbd resource."
  (let ((snd nil))
    (with-pstrs ((name name-string))
      (setq snd (#_getnamedresource "snd " name))
      (when detach? (#_detachresource snd)))
    snd))

(defun get-sound-from-file (name-string resfile 
                                        &optional (detach? t))
  "Gets a 'snd ' resource from the specified resource file.
If optional detach? is t, calls #_detachresource. This should
be left alone, or else the sound handle will be useless when the 
resource file is closed."
  (with-open-resource-file (refnumvar resfile)
    (get-sound name-string detach?)))

(defun UI-init-snd-channels ()
  (rlet ((c1 :ptr (%null-ptr))
         (c2 :ptr (%null-ptr))
         (c3 :ptr (%null-ptr))
         (c4 :ptr (%null-ptr)))
    (require-trap #_SndNewChannel c1 #$sampledSynth #$initStereo (%null-ptr))
    (require-trap #_SndNewChannel c2 #$sampledSynth #$initStereo (%null-ptr))
    (require-trap #_SndNewChannel c3 #$sampledSynth #$initStereo (%null-ptr))
    (require-trap #_SndNewChannel c4 #$sampledSynth #$initStereo (%null-ptr))
    (setf *UI-snd-channel-1* (%get-ptr c1))
    (setf *UI-snd-channel-2* (%get-ptr c2))
    (setf *UI-snd-channel-3* (%get-ptr c3))
    (setf *UI-snd-channel-4* (%get-ptr c4))))

(defun UI-close-all-snd-channels ()
  (dolist (c (list *UI-snd-channel-1* *UI-snd-channel-2* 
                   *UI-snd-channel-3* *UI-snd-channel-4*))
    (require-trap #_SndDisposeChannel c nil)
    (#_disposptr c)
    (setf c nil)))


#|
(ui-init-snd-channels)
(with-open-resource-file (refnumvar *Noodle-Rsrc-File*)
  (play-snd-async (get-sound "metal stamp") 
                   *Ui-Snd-Channel-1*))

(setq foo (get-sound "quack"))
(play-snd-sync foo)
(play-snd-async foo *Ui-Snd-Channel-1*)

; (ui-close-all-snd-channels)
|#