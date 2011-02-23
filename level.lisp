(in-package #:detome)

(make-object
 :name "background render"
 :render-level "background"
 :render-cb #'(lambda (obj)
                (declare (ignore obj))
                (draw-background)))

(make-object
 :name "base renderer"
 :render-level "base"
 :render-cb #'(lambda (obj)
                (declare (ignore obj))
                (draw-monsters)
                (draw-player)
                (draw-hover-messages)
                (draw-message-textarea *message-area-strings*)))

(make-object
 :name "bootstrap"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (build-rat-basement)
                (setf *primary-font* (sdl:initialise-default-font *primary-font-name*))
                (sdl:enable-alpha t :surface sdl:*default-display*)
                (sdl:enable-alpha t :surface sdl:*default-surface*)
                (sdl:enable-key-repeat 500 50)
                (define-images)
                (update-intensity-map (x *player*) (y *player*) 1.0)
                (clear-explored-map)
                ;;(populate-monsters)
                (set-render-order '("background" "base")))
 :update-cb-control :one-shot)

(make-object :name "global message receiver"
             :update-cb-control '(:ticks 1))

(defun detome (&optional (fullscreen nil))
  ;; reinit
  (set-render-order nil)
  (setf (update-cb-control (lookup-by-name "bootstrap")) :one-shot)

  (textarea-log '("Welcome to " (:color "ff0000") "Detome" (:color "ffffff") "! The goal of this game is to hunt down the dark wizard Varlok and have some good looting fun on the way.")
                :ttl 20)
  (textarea-log '("You awaken to rats. You have no recollection of how you got here but your head is aching.") :ttl 30)
  (mainloop :sdl-flags (if fullscreen sdl:sdl-fullscreen 0)))
