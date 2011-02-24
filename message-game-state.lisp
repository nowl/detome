(in-package #:detome)

(defparameter *pushed-values* nil)

(defparameter *message-textarea-window*
  (list 0
        0
        *screen-width*
        *screen-height*))

(defparameter *message-game-state* (make-instance 'game-state))
(switch *message-game-state*)

(set-render-order '("textarea"))

(make-object :name "event processor")
(add-to-broadcast-receivers (lookup-by-name "event processor") "sdl-event")

(set-meta (:sdl-event-cb "event processor")
  #'(lambda (event-type &rest args)
      (case event-type
        (:quit-event t)
        (:key-down-event
         (let ((key (cadr (member :key args))))
           (cond
             ((sdl:key= key :sdl-key-m)
              (setf *default-message-textarea-window* (pop *pushed-values*))
              (switch *play-game-state*)
              t))))
        (t nil))))

(switch *play-game-state*)

(defun switch-to-message-game-state ()
  (push *default-message-textarea-window* *pushed-values*)
  (setf *default-message-textarea-window* *message-textarea-window*)
  (switch *message-game-state*))