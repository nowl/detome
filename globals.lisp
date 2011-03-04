(in-package #:detome)

(defparameter *map-window* '(0 0 32 24)
  "The viewport of the map on the screen in terms of tiles. The
  ordering goes x and y location of the upper left corner of the map
  then width and height follow.")

;; messages

(defparameter *message-textarea-width-offset* 5
  "The number of pixels to indent the text from both the left and
  right sides.")
(defparameter *message-textarea-height-header-offset* 5
  "The space to leave at the top of the message area.")
(defparameter *message-textarea-height-footer-offset* 5
  "The space to leave at the bottom of the message area.")
(defparameter *message-textarea-height-offset-between* 2
  "The space to leave between each separate line.")
(defparameter *message-textarea-height-offset-between-messages* 5
  "The space to leave between each separate message.")
(defparameter *default-message-ttl-sec* 5
  "The default time to live for a message if a time is not provided.")


;; (defparameter *default-message-textarea-window*
;;   (list (* 32 (nth 2 *map-window*))
;;         0
;;         (- *screen-width* (* 32 (nth 2 *map-window*)))
;;         *screen-height*)
;;   "This defines the rightmost region of the screen where text messages may appear.")

(defparameter *default-message-textarea-window*
  (list 0
        0
        (- *screen-width* 150)
        *screen-height*))

(defparameter *draw-textarea-window* nil)
(defparameter *draw-message-exclamation* nil)
(defparameter *seconds-to-remove-message-notification* 3)

(defparameter *health-placement* `(,(- *screen-width* 250) ,(+ 32 16)))
(defparameter *seconds-for-health-flash* 3)

;; (defparameter *default-message-textarea-window*
;;   (list 0
;;         (* 32 (nth 3 *map-window*))
;;         *screen-width*
;;         (- *screen-height* (* 32 (nth 3 *map-window*))))
;;   "This defines the rightmost region of the screen where text messages may appear.")

;; fonts

(defparameter *primary-font* nil)
(defparameter *primary-font-name* sdl:*font-7x13*)
(defparameter *primary-font-width* 7)
(defparameter *primary-font-height* 13)

(defparameter *larger-font* nil)
(defparameter *larger-font-name* sdl:*font-9x15*)
(defparameter *larger-font-width* 9)
(defparameter *larger-font-height* 15)


;; light and LOS

(defparameter *light-intensity-cutoff* 0.1
  "The intensity under which LOS is assumed to be zero.")

;; environment related

(defparameter *day-night-cycle-in-seconds* (* 60 10)
  "The number of seconds of real game time that daytime and nighttime lasts.")

(defparameter *environment* :outside)
(defparameter *weather* :clear)
(defparameter *atmosphere* :day)

;; these have additive effects on the light attenuation
(defparameter *weather-attens*
  '(:clear 0
    :drizzle 0.1
    :light-rain 0.2
    :heavy-rain 0.4
    :storming 0.5
    :light-snow 0.6
    :heavy-snow 0.7
    :sand-storm 0.9))

(defparameter *atmosphere-attens*
  '(:day 0
    :night 0.7))

;; logging

(set-log-level :debug)

;; game states

(defparameter *play-game-state* (make-instance 'game-state))
(switch *play-game-state*)
