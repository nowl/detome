(in-package #:detome)

#|
(make-object
 :name "player death detector"
 :update-cb #'(lambda (obj)
                (declare (ignore obj))
                (when (<= (hp *player*) 0)
                  (reset-player)
                  (build-open-plains))))
|#

(defmethod bf:init ((obj (eql 'detome)))
  (bf:log :info "game initialization")
  (setf *primary-font* (sdl:initialise-default-font *primary-font-name*))
  (setf *larger-font* (sdl:initialise-default-font *larger-font-name*))
  (sdl:enable-alpha t :surface sdl:*default-display*)
  ;;(sdl:enable-alpha t :surface sdl:*default-surface*)
  (sdl:enable-key-repeat 500 50)
  (define-images)
  (reset-player)
  (build-open-plains))


;; This is the main entry point
(defun detome (&optional fullscreen)
  ;; reinit
  ;;;(set-render-order nil)
  ;;;(setf (update-cb-control (lookup-by-name "bootstrap")) :one-shot)

  (textarea-log '("Welcome to " (:color "ff0000") "Detome" (:color "ffffff") "!"))
  (textarea-log '("Find the legendary Detome."))
  (bf:mainloop 'detome :ms-per-update (/ 1000 *game-ticks-per-second*)
               :sdl-flags (if fullscreen sdl:sdl-fullscreen 0)))

(defparameter *update-inbox* nil)

;; messages directed to the update function
(defmethod bf:process-message ((obj (eql :update)) message delivery-type)
  (etypecase (bf:message-payload message)
    (function (push (bf:message-payload message)
                    *update-inbox*))))

(defmethod bf:update ((obj (eql 'detome)) tick)
  (loop for message in *update-inbox* do
       (funcall message))
  (setf *update-inbox* nil))

(defmethod bf:render ((obj (eql 'detome)) interpolation)
  (draw-background)
  ;;(draw-monsters)
  ;;(draw-items)
  (draw-player)
  ;;(draw-hover-messages)
  ;;(draw-hud)
  (when *draw-textarea-window*
    (draw-message-textarea *message-area-strings*))
  ;;(draw-notifications))
  )
