(in-package #:blackfish)

(defgeneric init (obj)
  (:documentation
   "This will be called on the specific object after SDL initialization."))

(defgeneric render (obj interpolation)
  (:documentation
   "This represents the act of 'rendering' a specific object."))

(defgeneric update (obj tick)
  (:documentation
   "This represents the act of 'updating' a specific object."))

(defparameter *loops* 0)
(defparameter *game-tick* 0)
(defparameter *next-update-in-ms* 0)
(defparameter *screen-height* 600)
(defparameter *screen-width* 800)
(defparameter *ms-per-update* (/ 1000 15))
(defparameter *max-frame-skip* 5)

(defun get-tick-count ()  
  (coerce (truncate (system-ticks)) 'fixnum))

(defun main-render (object interpolation)
  (fill-surface *black*)
  (render object interpolation)
  (update-display *default-surface*)
  (blit-surface *default-surface* *default-display*)
  (update-display))

(defun main-update (object)
  (incf *game-tick*)
  (update object *game-tick*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-event-form (sdl-event-name args)
	`(,sdl-event-name ,args
                      (send-message 
                       :sender :blackfish-main-loop
                       :receiver nil
                       :type :sdl-event
                       :payload (list ,sdl-event-name ,@args)
                       :delivery-type :async))))

(defmacro gen-idle-event ()
  (let ((current-time (gensym)))
    `(let ((,current-time (get-tick-count)))
       (if (and (> ,current-time *next-update-in-ms*)
		(< *loops* *max-frame-skip*))
	   (progn
	     (incf *loops*)
	     (incf *next-update-in-ms* *ms-per-update*)
	     (main-update object))
	   (progn
	     (setf *loops* 0)
	     (main-render object (float (/ (- ,current-time (- *next-update-in-ms* *ms-per-update*)) *ms-per-update*))))))))

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

(defun mainloop (object
                 &key (init-width *screen-width*) (init-height *screen-height*)
                 (ms-per-update *ms-per-update*) (max-frame-skip *max-frame-skip*)
                 (sdl-flags 0) title)
  (setf *screen-width* init-width
        *screen-height* init-height
        *ms-per-update* (float ms-per-update)
        *max-frame-skip* max-frame-skip)
  (let ((total-flags (logior sdl-flags sdl-sw-surface)))
	(with-init ()
	  (window *screen-width* *screen-height* :flags total-flags :title-caption title)
      (init object)
	  (setf *default-surface* (create-surface *screen-width* *screen-height*)
			*game-tick* 0
            *loops* 0
			*next-update-in-ms* (+ (get-tick-count) *ms-per-update*))
	  (setf (frame-rate) 0)
	  (gen-sdl-with-events))))
