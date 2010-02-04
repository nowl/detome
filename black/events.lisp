(in-package #:black)

(export '(add-quit-event
          add-key-down-event
		  add-key-up-event))

(defun add-quit-event (event)
  (pushnew event *event-list-quit-event*))

(defun add-key-down-event (event)
  (pushnew event *event-list-key-down-event*))

(defun add-key-up-event (event)
  (pushnew event *event-list-key-up-event*))