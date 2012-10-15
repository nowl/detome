(in-package :blacker)

(export '(mainloop
          *fps-counter*
          *game-tick*
          *screen-width*
          *screen-height*
          *ms-per-update*
          clear-engine-stats))

(defparameter *loops* 0)
(defparameter *game-tick* 0)
(defparameter *next-update-in-ms* 0)
(defparameter *screen-height* 1024)
(defparameter *screen-width* 1280)
(defparameter *ms-per-update* (/ 1000 25))
(defparameter *max-frame-skip* 5)

(defun clear-engine-stats ()
  (setf *game-tick* 0
        *next-update-in-ms* 0
        *loops* 0))

(defun get-tick-count ()  
  (coerce (truncate (system-ticks)) 'fixnum))

(defparameter *fps-counter* 0)
(defparameter *last-tick* (get-tick-count))

(defun main-render (interpolation)
  (when (> (- (get-tick-count) *last-tick*) 1000)
    (setf *last-tick* (get-tick-count))
          *fps-counter* 0)
  (incf *fps-counter*)
  (gl:clear :color-buffer-bit)
  (send-message :system-render interpolation :async)
  (update-display))

(defun main-update ()
  (incf *game-tick*)
  (send-message :system-update)
  (process-messages))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-event-form (sdl-event-name args)
	`(,sdl-event-name ,args
                      (send-message
                       :sdl-event
                       (list ,sdl-event-name ,@args)
                       :async))))

(defmacro gen-idle-event ()
  (let ((current-time (gensym)))
    `(let ((,current-time (get-tick-count)))
       (if (and (> ,current-time *next-update-in-ms*)
		(< *loops* *max-frame-skip*))
	   (progn
	     (incf *loops*)
	     (incf *next-update-in-ms* *ms-per-update*)
	     (main-update))
	   (progn
	     (setf *loops* 0)
	     (main-render (float (/ (- ,current-time (- *next-update-in-ms* *ms-per-update*)) *ms-per-update*))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *event-list-data*
    '((:active-event (:gain gain :state state))
      (:key-down-event (:key key :mod mod :mod-key mod-key))
      (:key-up-event (:key key :mod mod :mod-key mod-key))
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

(defun mainloop (&key (init-width *screen-width*) (init-height *screen-height*)
                 (ms-per-update *ms-per-update*) (max-frame-skip *max-frame-skip*)
                 (sdl-flags 0) title)
  (setf *screen-width* init-width
        *screen-height* init-height
        *ms-per-update* (float ms-per-update)
        *max-frame-skip* max-frame-skip)
  (let ((total-flags (logior sdl-flags sdl-opengl))) ;;sdl-sw-surface)))
	(with-init ()
	  (window *screen-width* *screen-height* :flags total-flags :title-caption title)

      (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

      (sdl:set-gl-attribute :sdl-gl-red-size 5)
      (sdl:set-gl-attribute :sdl-gl-green-size 5)
      (sdl:set-gl-attribute :sdl-gl-blue-size 5)
      (sdl:set-gl-attribute :sdl-gl-doublebuffer 1)
      (gl:enable :line-smooth)
      (gl:disable :depth-test)
      ;;(gl:enable :texture-2d)

	  (setf *default-surface* (create-surface *screen-width* *screen-height*)
			*game-tick* 0
            *loops* 0
			*next-update-in-ms* (+ (get-tick-count) *ms-per-update*))

      (gl:clear-color 0 0 0 0)
      (gl:viewport 0 0 *screen-width* *screen-height*)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 *screen-width* *screen-height* 0 0 1)
      (gl:matrix-mode :modelview)
      (gl:load-identity)

      (send-message :system-init nil :async)

	  (setf (frame-rate) 0)
	  (gen-sdl-with-events))))
