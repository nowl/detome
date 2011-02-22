(in-package #:black)

(export '(*screen-width*
          *screen-height*
          *game-tick*
          *render-tick*
          *interpolation*
		  reset-globals))

(defparameter *updates-per-second* 15)
(defparameter *ms-per-update* (/ 1000 *updates-per-second*))
(defparameter *max-frame-skip* 5)

(defparameter *screen-width* 1024)
(defparameter *screen-height* 768)

(defparameter *game-tick* 0)
(defparameter *render-tick* 0)
(defparameter *interpolation* 0)

(defparameter *log-types-to-print*
  '(:error
    :warning
    :info))