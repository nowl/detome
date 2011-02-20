(in-package #:black)

(export '(*screen-width*
          *screen-height*
		  reset-globals))

(defvar *updates-per-second* 15)
(defvar *ms-per-update* (/ 1000 *updates-per-second*))
(defvar *max-frame-skip* 5)

(defvar *screen-width* 1024)
(defvar *screen-height* 768)

(defvar *game-tick* 0)