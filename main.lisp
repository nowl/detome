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
                (draw-items)
                (draw-player)
                (draw-hover-messages)))

(make-object
 :name "textarea renderer"
 :render-level "textarea"
 :render-cb #'(lambda (obj)
                (declare (ignore obj))
                (when *draw-textarea-window*
                  (draw-message-textarea *message-area-strings*))))

(make-object
 :name "player death detector"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (when (<= (hp *player*) 0)
                  (reset-player)
                  (build-rat-basement))))

;;(add (lookup-by-name "textarea renderer") *message-game-state*)

;; This is a one-shot updater that sets up some various sdl-specific
;; things that are required after sdl-init and also serves to
;; bootstrap the game and enter the first level.
(make-object
 :name "bootstrap"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (setf *primary-font* (sdl:initialise-default-font *primary-font-name*))
                (setf *larger-font* (sdl:initialise-default-font *larger-font-name*))
                (sdl:enable-alpha t :surface sdl:*default-display*)
                (sdl:enable-alpha t :surface sdl:*default-surface*)
                (sdl:enable-key-repeat 500 50)
                (define-images)
                (reset-player)
                ;;(update-intensity-map (x *player*) (y *player*) 1.0)
                ;;(clear-explored-map)
                ;;(populate-monsters)
                (set-render-order '("background"
                                    "base"
                                    "hud"
                                    "textarea"
                                    "notifications"))
                ;;(build-rat-basement))
                ;;(build-random-cave 50 50 50 1))
                (build-open-plains))
 
 :update-cb-control :one-shot)

(make-object :name "global message receiver"
             :update-cb-control '(:ticks 1))

;; This is the main entry point. It should do any initialization
;; before entering the mainloop in black.
(defun detome (&optional (fullscreen nil))
  ;; reinit
  (set-render-order nil)
  (setf (update-cb-control (lookup-by-name "bootstrap")) :one-shot)

  (textarea-log '("Welcome to " (:color "ff0000") "Detome" (:color "ffffff") "!"))
  (textarea-log '("Journey to the bottom of a level 6 dungeon and find the legendary Detome."))
  (mainloop :sdl-flags (if fullscreen sdl:sdl-fullscreen 0)))
