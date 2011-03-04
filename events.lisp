(in-package #:detome)

;; The map window should stay relatively centered on the player unless
;; we are near an edge in which case the map window fills the view.
(defun move-map-window-if-needed ()
  (let ((window-width (nth 2 *map-window*))
        (window-height (nth 3 *map-window*)))

    ;; handle x direction
    (cond ((and (eq *level-type* :predefined) 
                (< (x *player*) (/ window-width 2)))
           (setf (nth 0 *map-window*) 0))
          ((and (eq *level-type* :predefined)
                (> (x *player*) (- *level-width* (/ window-width 2))))
           (setf (nth 0 *map-window*) (- *level-width* window-width)))
          (t (setf (nth 0 *map-window*) (- (x *player*) (/ window-width 2)))))

    ;; handle y direction
    (cond ((and (eq *level-type* :predefined)
                (< (y *player*) (/ window-height 2)))
           (setf (nth 1 *map-window*) 0))
          ((and (eq *level-type* :predefined)
                (> (y *player*) (- *level-height* (/ window-height 2))))
           (setf (nth 1 *map-window*) (- *level-height* window-height)))
          (t (setf (nth 1 *map-window*) (- (y *player*) (/ window-height 2)))))))


(defun take-turn ()
  "Any objects that use :turns will have their number of turns incremented."
  (every-object #'(lambda (obj)
                    (with-slots (update-cb-control) obj
                      (when (and (consp update-cb-control) (eq (car update-cb-control) :turns))
                        (incf (second update-cb-control)))))))

(defun attempt-move-player (delta-x delta-y)
  (with-slots (x y) *player*
    (let* ((new-x (+ x delta-x))
           (new-y (+ y delta-y))
           (monsters (monsters-at new-x new-y)))
	  (multiple-value-bind (walkable walkable-reason) (walkable new-x new-y)
		(cond ((and (not walkable)
					(not (eq walkable-reason :actor)))
			   ;; we're blocked and not due to a monster or another
			   ;; actor
			   (textarea-log `("Blocked going " (:color "0000ff") 
												,(ecase delta-x
														(1 (ecase delta-y
															 (0 "east")
															 (1 "southeast")
															 (-1 "northeast")))
														(0 (ecase delta-y
															 (1 "south")
															 (-1 "north")))
														(-1 (ecase delta-y
															  (1 "southwest")
															  (0 "west")
															  (-1 "northwest"))))
												(:color ,sdl:*white*) "!")))
			  (monsters (attack *player* (typecase monsters
										   (cons (first monsters))
										   (t monsters)))
						(take-turn))
			  (t (setf x new-x y new-y)
                 (take-turn)))))))

(defmacro gen-move-command (key-symbol delta-x delta-y)
  ``((sdl:key= key ,,key-symbol)
     (make-and-send-message 
      :sender "event processor" :receiver "global message receiver"
      :action #'(lambda (sender receiver type)
                  (declare (ignore sender receiver type))
                  (attempt-move-player ,,delta-x ,,delta-y)
                  (move-map-window-if-needed)
                  (update-intensity-map (x *player*) (y *player*) 1.0)))
     t))

(make-object :name "event processor")

(add-to-broadcast-receivers (lookup-by-name "event processor") "sdl-event")

(set-meta (:sdl-event-cb "event processor")
  #'(lambda (event-type &rest args)
      (case event-type
        (:quit-event t)
        (:key-down-event
         (let ((key (cadr (member :key args))))
           (cond
             ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event) t)
             #.(gen-move-command :sdl-key-kp4 -1 0)
             #.(gen-move-command :sdl-key-kp7 -1 -1)
             #.(gen-move-command :sdl-key-kp8 0 -1)
             #.(gen-move-command :sdl-key-kp9 1 -1)
             #.(gen-move-command :sdl-key-kp6 1 0)
             #.(gen-move-command :sdl-key-kp3 1 1)
             #.(gen-move-command :sdl-key-kp2 0 1)
             #.(gen-move-command :sdl-key-kp1 -1 1)
             ((sdl:key= key :sdl-key-e)
              (loop for item in *items-in-level* do
                   (when (and (= (x item) (x *player*))
                              (= (y item) (y *player*)))
                     (funcall (cb (item-type item)) item *player*)
                     (setf *items-in-level* (delete item *items-in-level*)))))
             ((sdl:key= key :sdl-key-space)
              (setf *draw-textarea-window* (not *draw-textarea-window*)
                    *draw-message-exclamation* nil)))))
        (t nil))))
