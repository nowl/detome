(in-package #:black)

(export '(*screen-width*
          *screen-height*
		  reset-globals))

(defvar *updates-per-second* 15)
(defvar *ms-per-update* (/ 1000 *updates-per-second*))
(defvar *max-frame-skip* 5)

(defvar *screen-width* 1024)
(defvar *screen-height* 768)

(defvar *render-list* nil)

;; this is an alist of modes mapping to lists of event handling
;; functions
(defvar *event-mode-hooks* nil)

;; these are all current lists of functions implementing each event
;; type
(defvar *event-list-active-event* nil)
(defvar *event-list-key-down-event* nil)
(defvar *event-list-key-up-event* nil)
(defvar *event-list-mouse-motion-event* nil)
(defvar *event-list-mouse-button-down-event* nil)
(defvar *event-list-mouse-button-up-event* nil)
(defvar *event-list-joy-axis-motion-event* nil)
(defvar *event-list-joy-button-down-event* nil)
(defvar *event-list-joy-button-up-event* nil)
(defvar *event-list-joy-hat-motion-event* nil)
(defvar *event-list-joy-ball-motion-event* nil)
(defvar *event-list-video-resize-event* nil)
(defvar *event-list-video-expose-event* nil)
(defvar *event-list-sys-wm-event* nil)
(defvar *event-list-quit-event* nil)
(defvar *event-list-user-event* nil)

(defvar *object-list* nil)
(defvar *object-name-lookup* (make-hash-table :test #'equal))

(defun reset-globals ()
  (setf *screen-width* 1024
        *screen-height* 768
        *render-list* nil
        *object-list* nil       
        *object-name-lookup* (make-hash-table :test #'equal)
        *event-mode-hooks* nil
        *event-list-active-event* nil
        *event-list-key-down-event* nil
        *event-list-key-up-event* nil
        *event-list-mouse-motion-event* nil
        *event-list-mouse-button-down-event* nil
        *event-list-mouse-button-up-event* nil
        *event-list-joy-axis-motion-event* nil
        *event-list-joy-button-down-event* nil
        *event-list-joy-button-up-event* nil
        *event-list-joy-hat-motion-event* nil
        *event-list-joy-ball-motion-event* nil
        *event-list-video-resize-event* nil
        *event-list-video-expose-event* nil
        *event-list-sys-wm-event* nil
        *event-list-quit-event* nil
        *event-list-user-event* nil))	
