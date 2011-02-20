(in-package #:black)

(export '(mainloop
		  reset-game))

(defvar *loops* 0)
(defvar *next-update-in-ms* 0)

(defun get-tick-count ()  
  (coerce (truncate (system-ticks)) 'fixnum))

(defun main-render (interpolation)
  (setf *interpolation* interpolation)
  (incf *render-tick*)
  (fill-surface *black*)
  (render *game-state*)
  (update-display *default-surface*)
  (blit-surface *default-surface* *default-display*)
  (update-display))

(defun main-update ()  
  (incf *game-tick*)
  (process-messages)
  (update *game-state*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-event-form (sdl-event-name args)
	`(,sdl-event-name ,args
                      (make-and-send-message 
                       :sender "main-loop"
                       :receiver nil
                       :mes-type "sdl-event"
                       :action #'(lambda (sender receiver type)
                                   (multiple-value-bind (func hit) (gethash :sdl-event-cb (meta receiver))
                                     (if hit
                                         (funcall #'func ,sdl-event-name)
                                         nil)))
                       :type :async))))

(defmacro gen-idle-event ()
  (let ((current-time (gensym)))
    `(let ((,current-time (get-tick-count)))
       (if (and (> ,current-time *next-update-in-ms*)
		(< *loops* *max-frame-skip*))
	   (progn
	     (incf *loops*)
	     (incf *next-update-in-ms* *ms-per-update*)
	     (update))
	   (progn
	     (setf *loops* 0)
	     (main-render (float (/ (- ,current-time (- *next-update-in-ms* *ms-per-update*)) *ms-per-update*))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *event-list-data*
    '((:active-event (:gain gain :state state))
      (:key-down-event (:state state :scancode scancode :key key :mod mod :mod-key mod-key :unicode unicode))
      (:key-up-event (:state state :scancode scancode :key key :mod mod :mod-key mod-key :unicode unicode))
      (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel yrel))
      (:mouse-button-down-event (:button button :state state :x x :y y))
      (:mouse-button-up-event (:button button :state state :x x :y y))
      (:joy-axis-motion-event (:which which :axis axis :value value))
      (:joy-button-down-event (:which which :button button :state state))
      (:joy-button-up-event (:which which :button button :state state))
      ;;(:joy-hat-motion-event (:which which :hat hat :value value) *event-list-joy-hat-motion-event*)
      (:joy-ball-motion-event (:which which :ball ball :x-rel x-rel :y-rel y-rel))
      (:video-resize-event (:w w :h h))
      (:video-expose-event ())
      (:sys-wm-event ())
      (:user-event (:type type :code code :data1 data1 :data2 data2))
      (:quit-event ()))))

(defmacro gen-sdl-with-events ()
  `(with-events (:poll)
     ,@(loop for event in *event-list-data* collect
            (gen-event-form (first event) (second event)))
     (:idle () (gen-idle-event))))

(defun mainloop (&key (sdl-flags 0) title)
  (let ((total-flags (logior sdl-flags sdl-sw-surface)))
	(with-init ()
	  (window *screen-width* *screen-height* :flags total-flags :title-caption title)
	  (setf *default-surface* (create-surface *screen-width* *screen-height*)
			*game-tick* 0
			*render-tick* 0
			*loops* 0
			*next-update-in-ms* (+ (get-tick-count) *ms-per-update*))
	  (setf (frame-rate) 0)    
	  (gen-sdl-with-events))))
