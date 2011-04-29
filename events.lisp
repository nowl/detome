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
               #|
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
               |#
               )
			  (monsters (attack *player* (typecase monsters
										   (cons (first monsters))
										   (t monsters)))
						(take-turn))
			  (t (setf x new-x y new-y)
                 (take-turn)))))))

(defun check-for-item-pickup ()
  (loop for item in *items-in-level* do
       (when (and (= (x item) (x *player*))
                  (= (y item) (y *player*)))
         (funcall (cb (item-type item)) item *player*)
         (setf *items-in-level* (delete item *items-in-level*)))))

(defun check-for-cave-entrance ()
  (let ((cave-number (map-cell-number (gethash "cave" *map-cells-by-name*))))
    (when (member cave-number (get-map-points (x *player*) (y *player*)))
      (textarea-log (list "Discovered a cave entrance!"))
      (textarea-log `("Press " (:color "ff0000") "e" (:color ,sdl:*white*) " to enter this cave.")))))

(defun check-for-stairs-up-entrance ()
  (let ((stairs-up-number (map-cell-number (gethash "stairs-up" *map-cells-by-name*))))
    (when (member stairs-up-number (get-map-points (x *player*) (y *player*)))
      (textarea-log (list "Discovered a stairway up!"))
      (textarea-log `("Press " (:color "ff0000") "e" (:color ,sdl:*white*) " to ascend.")))))

(defun check-for-stairs-down-entrance ()
  (let ((stairs-down-number (map-cell-number (gethash "stairs-down" *map-cells-by-name*))))
    (when (member stairs-down-number (get-map-points (x *player*) (y *player*)))
      (textarea-log (list "Discovered a stairway down!"))
      (textarea-log `("Press " (:color "ff0000") "e" (:color ,sdl:*white*) " to descend.")))))

(defun build-trade-orders (trader)
  (with-output-to-string (str)
    (write-string "Will trade " str)
    (let ((trade (cadr trader)))
      (when (plusp (first trade)) (format str "~d provisions " (first trade)))
      (when (plusp (second trade)) (format str "~d defense " (second trade)))
      (when (plusp (third trade)) (format str "~d attack " (third trade)))
      (when (plusp (fourth trade)) (format str "~d damage " (fourth trade)))
      (when (plusp (fifth trade)) (format str "~d health " (fifth trade))))
    (write-string "for " str)
    (let ((trade (car trader)))
      (when (plusp (first trade)) (format str "~d green " (first trade)))
      (when (plusp (second trade)) (format str "~d blue " (second trade)))
      (when (plusp (third trade)) (format str "~d red " (third trade)))
      (when (plusp (fourth trade)) (format str "~d yellow " (fourth trade))))))

(defun check-for-trader ()
  (let ((trader-number (map-cell-number (gethash "trader" *map-cells-by-name*))))
    (when (member trader-number (get-map-points (x *player*) (y *player*)))
      (let ((trader (gethash (list (x *player*) (y *player*)) *discovered-traders*)))
        (when (not trader)
          (textarea-log '("Discovered a new trader!"))
          (setf trader (random-choice *traders*)
                (gethash (list (x *player*) (y *player*)) *discovered-traders*) trader))
        (textarea-log (list (build-trade-orders trader)))
        (textarea-log `("Press " (:color "ff0000") "e" (:color ,sdl:*white*) " to make this trade."))))))

(defun test-for-action-at-location ()
  ;; test for trader
  (let ((trader-number (map-cell-number (gethash "trader" *map-cells-by-name*))))
    (when (member trader-number (get-map-points (x *player*) (y *player*)))
      (let ((trader (gethash (list (x *player*) (y *player*)) *discovered-traders*)))
        (let ((trade (car trader)))
          (when (plusp (first trade))
            (when (< (g-energy *player*) (first trade))
              (return-from test-for-action-at-location nil)))
          (when (plusp (second trade))
            (when (< (b-energy *player*) (second trade))
              (return-from test-for-action-at-location nil)))
          (when (plusp (third trade))
            (when (< (r-energy *player*) (third trade))
              (return-from test-for-action-at-location nil)))
          (when (plusp (fourth trade))
            (when (< (y-energy *player*) (fourth trade))
              (return-from test-for-action-at-location nil)))

          ;; can afford this
          (when (plusp (first trade))
            (decf (g-energy *player*) (first trade)))
          (when (plusp (second trade))
            (decf (b-energy *player*) (second trade)))
          (when (plusp (third trade))
            (decf (r-energy *player*) (third trade)))
          (when (plusp (fourth trade))
            (decf (y-energy *player*) (fourth trade))))

        ;; get the goods
        (let ((trade (cadr trader)))
          (when (plusp (first trade))
            (incf (provisions *player*) (first trade)))
          (when (plusp (second trade))
            (setf (def-r *player*)
                  (list (+ (first (def-r *player*)) (second trade))
                        (+ (second (def-r *player*)) (second trade)))))
          (when (plusp (third trade))
            (setf (att-r *player*)
                  (list (+ (first (att-r *player*)) (third trade))
                        (+ (second (att-r *player*)) (third trade)))))
          (when (plusp (fourth trade))
            (setf (dmg-r *player*)
                  (list (+ (first (dmg-r *player*)) (fourth trade))
                        (+ (second (dmg-r *player*)) (fourth trade)))))
          (when (plusp (fifth trade))
            (incf (hp-max *player*) (fifth trade)))))))

  ;; test for stairs down
  (let ((stairs-down-number (map-cell-number (gethash "stairs-down" *map-cells-by-name*))))
    (when (member stairs-down-number (get-map-points (x *player*) (y *player*)))
      (incf *d-level*)
      (build-random-cave (+ 20 (random 100))
                         (+ 20 (random 100))
                         (+ 50 (random 200))
                         *d-level*)))

  ;; test for stairs up
  (let ((stairs-up-number (map-cell-number (gethash "stairs-up" *map-cells-by-name*))))
    (when (member stairs-up-number (get-map-points (x *player*) (y *player*)))
      (decf *d-level*)
      (if (= *d-level* 0)
          (progn
            (build-open-plains)
            (place-player (first *player-cave-entrance-location*)
                          (second *player-cave-entrance-location*)))            
          (build-random-cave (+ 20 (random 100))
                             (+ 20 (random 100))
                             (+ 50 (random 200))
                             *d-level*))))
  
  ;; test for cave entrance
  (let ((cave-number (map-cell-number (gethash "cave" *map-cells-by-name*))))
    (when (member cave-number (get-map-points (x *player*) (y *player*)))
      (cleanup-open-plains)
      (setf *player-cave-entrance-location* (list (x *player*) (y *player*)))
      (setf *d-level* 1)
      (build-random-cave (+ 20 (random 100))
                         (+ 20 (random 100))
                         (+ 50 (random 200))
                         *d-level*)))


  t)


(defmacro gen-move-command (key-symbol delta-x delta-y)
  ``((sdl:key= key ,,key-symbol)
     (make-and-send-message 
      :sender "event processor" :receiver "global message receiver"
      :action #'(lambda (sender receiver type)
                  (declare (ignore sender receiver type))
                  (attempt-move-player ,,delta-x ,,delta-y)
                  (check-for-item-pickup)
                  (check-for-trader)
                  (check-for-cave-entrance)
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
             ((sdl:key= key :sdl-key-e) (test-for-action-at-location) t)
             #.(gen-move-command :sdl-key-kp4 -1 0)
             #.(gen-move-command :sdl-key-kp7 -1 -1)
             #.(gen-move-command :sdl-key-kp8 0 -1)
             #.(gen-move-command :sdl-key-kp9 1 -1)
             #.(gen-move-command :sdl-key-kp6 1 0)
             #.(gen-move-command :sdl-key-kp3 1 1)
             #.(gen-move-command :sdl-key-kp2 0 1)
             #.(gen-move-command :sdl-key-kp1 -1 1))))
        (t nil))))
