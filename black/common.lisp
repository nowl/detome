(in-package #:black)

(export 'common-quit-event)

(defun common-quit-event ()
  "The standard event that should be added to *event-list-quit-event*
to quit the sdl app."
  t)