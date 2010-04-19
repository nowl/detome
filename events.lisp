(in-package #:detome)

(add-quit-event #'common-quit-event)

(defun press-escape-to-quit-event (&key key &allow-other-keys)
  (cond ((sdl:key= key :sdl-key-escape)
         (sdl:push-quit-event)
         t)
        (t nil)))

(add-key-down-event #'press-escape-to-quit-event)

(defun move-map-window-if-needed ()
  ;; The map window should stay relatively centered on the player
  ;; unless we are near an edge in which case the map window fills the
  ;; view.

  (let ((level-height (array-dimension *level* 0))
	(level-width (array-dimension *level* 1))
	(window-width (nth 2 *map-window*))
	(window-height (nth 3 *map-window*)))

    ;; handle x direction
    (cond ((< (x *player*) (/ window-width 2))
	   (setf (nth 0 *map-window*) 0))
	  ((> (x *player*) (- level-width (/ window-width 2)))
	   (setf (nth 0 *map-window*) (- level-width window-width)))
	  (t (setf (nth 0 *map-window*) (- (x *player*) (/ window-width 2)))))

    ;; handle y direction
    (cond ((< (y *player*) (/ window-height 2))
	   (setf (nth 1 *map-window*) 0))
	  ((> (y *player*) (- level-height (/ window-height 2)))
	   (setf (nth 1 *map-window*) (- level-height window-height)))
	  (t (setf (nth 1 *map-window*) (- (y *player*) (/ window-height 2)))))))


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
      :action #'(lambda (sender receiver)
                  (attempt-move-player ,,delta-x ,,delta-y)
                  (move-map-window-if-needed)
                  (update-intensity-map (x *player*) (y *player*) 1.0)))
     t))

#|
(mid-displace 10 10 :array *level* :roughness 100.0 
									  :post-filter-func #'(lambda (val)
															(map-cell-number (gethash 
																			  (cond ((> val 0.85) "tree-on-mountain")
																					((> val 0.65) "mountain")
																					((> val 0.45) "tree-on-plain")
																					(t "plain"))
																			  *map-cells-by-name*))))
						(update-intensity-map (x *player*) (y *player*) 1.0)))
|#

(defun scroll-map-with-arrows-event (&key key &allow-other-keys)
	(cond #.(gen-move-command :sdl-key-kp4 -1 0)
		  #.(gen-move-command :sdl-key-kp7 -1 -1)
		  #.(gen-move-command :sdl-key-kp8 0 -1)
		  #.(gen-move-command :sdl-key-kp9 1 -1)
		  #.(gen-move-command :sdl-key-kp6 1 0)
		  #.(gen-move-command :sdl-key-kp3 1 1)
		  #.(gen-move-command :sdl-key-kp2 0 1)
		  #.(gen-move-command :sdl-key-kp1 -1 1)
		  ((sdl:key= key :sdl-key-r)
		   (make-and-send-message
			:sender "event processor" :receiver "global message receiver"
			:action #'(lambda (sender receiver)
                        (let (start-pos)
                          (setf *level* (array-map (generate-dungeon (array-dimension *level* 0)
                                                                     (array-dimension *level* 1)
                                                                     100)
                                                   #'(lambda (tile i j)
                                                       (map-cell-number
                                                        (gethash
                                                         (case tile
                                                           (:wall "wall")
                                                           (:floor (when (null start-pos)
                                                                     (setf start-pos (list i j)))
                                                                   "plain")
                                                           (:hallway "plain")
                                                           (otherwise "wall"))
                                                         *map-cells-by-name*)))))
                          (setf (x *player*) (second start-pos)
                                (y *player*) (first start-pos))
                          (update-intensity-map (x *player*) (y *player*) 1.0))))
                                                     
		   t)
		  ((sdl:key= key :sdl-key-h)
		   (make-and-send-message 
			:sender "event processor" :receiver "global message receiver"
			:action #'(lambda (sender receiver)
						(add-health-hover)))
		   t)
		  ((sdl:key= key :sdl-key-j)
		   (make-and-send-message
			:sender "event processor" :receiver "global message receiver"
			:action #'(lambda (sender receiver)
						(add-damage-hover *player* -5)))
		   t)
		  (t nil)))


(add-key-down-event #'scroll-map-with-arrows-event)
