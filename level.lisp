(in-package #:detome)

(defun take-turn ()
  "Any objects that use :turns will have their number of turns incremented."
  (loop for obj in black::*object-list* do
       (with-slots (update-cb-control) obj
         (when (and (consp update-cb-control) (eq (car update-cb-control) :turns))
           (incf (second update-cb-control))))))

(define-object
    :name "primary renderer"
  :render-cb #'(lambda (obj interpolation)
                 (draw-background interpolation)
                 (draw-monsters interpolation)
                 (draw-player interpolation)
                 (draw-hover-messages interpolation)
                 (draw-message-textarea *message-area-strings* interpolation)))

(define-object
    :name "render updater"
  :update-cb #'(lambda (obj)
		 (setf *primary-font* (sdl:initialise-default-font *primary-font-name*))
		 (sdl:enable-alpha t :surface sdl:*default-display*)
		 (sdl:enable-alpha t :surface sdl:*default-surface*)
                 (sdl:enable-key-repeat 500 50)
		 (define-images)
		 (update-intensity-map (x *player*) (y *player*) 1.0)
		 (clear-explored-map)
		 (clear-render-list)		 
		 (populate-monsters)
		 (add-to-render-list "primary renderer"))
  :update-cb-control :one-shot)

(define-object :name "event processor")

(define-object :name "global message receiver"
  :update-cb-control '(:ticks 1))

(defun detome (&optional (fullscreen nil))
  ;; reinit
  (setf (update-cb-control (get-object-by-name "render updater")) :one-shot)

  (textarea-log '("Welcome to " (:color "ff0000") "Detome" (:color "ffffff") "! The goal of this game is to hunt down the dark wizard Varlok and have some good looting fun on the way.")
		:ttl 20)
  (mainloop :sdl-flags (if fullscreen sdl:sdl-fullscreen 0)))
