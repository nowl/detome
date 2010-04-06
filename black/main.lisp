(in-package #:black)

(export '(mainloop
		  reset-game))

(defvar *game-tick* 0)
(defvar *render-tick* 0)
(defvar *loops* 0)
(defvar *next-update-in-ms* 0)

(defun get-tick-count ()  
  (coerce (truncate (system-ticks)) 'fixnum))

(defun render (interpolation)
  (incf *render-tick*)
  (fill-surface *black*)
  (loop for obj in *render-list* do
       (render-obj obj interpolation))
  (update-display *default-surface*)
  (blit-surface *default-surface* *default-display*)
  (update-display))

(defun update ()  
  (incf *game-tick*)

  ;; deliver messages
  (route-messages)

  ;; update objects
  (loop for obj in *object-list* do
       (update-obj obj)))

(defmacro list-funcall (list &rest args)
  (let ((f (gensym)))
    `(dolist (,f ,list)
       (when (funcall ,f ,@args)
	 (return t)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-event-form (sdl-event-name args list-name)
	`(,sdl-event-name ,args (list-funcall ,list-name ,@args))))

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
	     (render (float (/ (- ,current-time (- *next-update-in-ms* *ms-per-update*)) *ms-per-update*))))))))
	

(defmacro gen-sdl-with-events ()
  (let ((event-lists
	 '((:active-event (:gain gain :state state) *event-list-active-event*)
	   (:key-down-event (:state state :scancode scancode :key key :mod mod :mod-key mod-key :unicode unicode) *event-list-key-down-event*)
	   (:key-up-event (:state state :scancode scancode :key key :mod mod :mod-key mod-key :unicode unicode) *event-list-key-up-event*)
	   (:mouse-motion-event (:state state :x x :y y :x-rel x-rel :y-rel yrel) *event-list-mouse-motion-event*)
	   (:mouse-button-down-event (:button button :state state :x x :y y) *event-list-mouse-button-down-event*)
	   (:mouse-button-up-event (:button button :state state :x x :y y) *event-list-mouse-button-up-event*)
	   (:joy-axis-motion-event (:which which :axis axis :value value) *event-list-joy-axis-motion-event*)
	   (:joy-button-down-event (:which which :button button :state state) *event-list-joy-button-down-event*)
	   (:joy-button-up-event (:which which :button button :state state) *event-list-joy-button-up-event*)
	   ;;(:joy-hat-motion-event (:which which :hat hat :value value) *event-list-joy-hat-motion-event*)
	   (:joy-ball-motion-event (:which which :ball ball :x-rel x-rel :y-rel y-rel) *event-list-joy-ball-motion-event*)
	   (:video-resize-event (:w w :h h) *event-list-video-resize-event*)
	   (:video-expose-event () *event-list-video-expose-event*)
	   (:sys-wm-event () *event-list-sys-wm-event*)
	   (:user-event (:type type :code code :data1 data1 :data2 data2) *event-list-user-event*)
	   (:quit-event () *event-list-quit-event*))))	   
    `(with-events (:poll)
       ,@(loop for event in event-lists collect
	      (gen-event-form (first event) (second event) (third event)))
       (:idle () (gen-idle-event)))))

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

(defun reset-game ()
  (reset-globals)
  (clear-render-list)
  (clear-image-caches))
  