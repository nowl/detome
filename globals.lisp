(in-package #:detome)

(defvar *map-window* '(0 0 24 24)
  "The viewport of the map on the screen in terms of tiles. The
  ordering goes x and y location of the upper left corner of the map
  then width and height follow.")

;; messages

(defvar *message-textarea-width-offset* 5
  "The number of pixels to indent the text from both the left and
  right sides.")
(defvar *message-textarea-height-header-offset* 5
  "The space to leave at the top of the message area.")
(defvar *message-textarea-height-footer-offset* 5
  "The space to leave at the bottom of the message area.")
(defvar *message-textarea-height-offset-between* 2
  "The space to leave between each separate line.")
(defvar *message-textarea-height-offset-between-messages* 5
  "The space to leave between each separate message.")
(defvar *default-message-ttl-sec* 10
  "The default time to live for a message if a time is not provided.")


(defvar *default-message-textarea-window* (list (* 32 (nth 2 *map-window*))
						0
						(- *screen-width* (* 32 (nth 2 *map-window*)))
						*screen-height*)
  "This defines the rightmost region of the screen where text messages may appear.")

;; fonts

(defvar *primary-font* nil)
(defvar *primary-font-name* sdl:*font-7x13*)
(defvar *primary-font-width* 7)
(defvar *primary-font-height* 13)

;; light and LOS

(defvar *light-intensity-cutoff* 0.1
  "The intensity under which LOS is assumed to be zero.")

;; weather related

(defvar *day-night-cycle-in-seconds* 30
  "The number of seconds of real game time that daytime and nighttime lasts.")

(defvar *environment* :outside)
(defvar *weather* :clear)
