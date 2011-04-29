(in-package #:detome)

(defun build-random-cave (width height complexity dlevel)
  ;(clear-intensity-map)
  (clear-monsters-from-level)
  (clear-scenery-from-level)
  (clear-items-from-level)
  
  (let ((map
         (let ((pre (generate-dungeon width height complexity)))
           (loop for i below (array-dimension pre 0) collect
                (loop for j below (array-dimension pre 1) collect
                     (if (eq (aref pre i j) :floor)
                         5
                         6))))))

    (setf *level-type* :predefined
          *level-width* (length map)
          *level-height* (length (car map))
          *level* (make-array (list (length (car map)) (length map))))
    (loop for i below *level-height* do
         (loop for j below *level-width* do
              (setf (aref *level* i j)
                    (list (nth i (nth j map)))))))


  ;; gather up floor spaces
  (let (floor-spaces)
    (loop for x below *level-width* do
         (loop for y below *level-height* do
              (when (equal (aref *level* y x) '(5))
                (push (list x y) floor-spaces))))

    ;; place random stairs down
    (let ((stairs-down-loc (random-choice floor-spaces))
          (stairs-down-number (map-cell-number (gethash "stairs-down" *map-cells-by-name*))))
      (setf (aref *level* (second stairs-down-loc) (first stairs-down-loc))
            (list 5 stairs-down-number)))
        
    (clear-intensity-map)
    (clear-explored-map)

    ;; set player at a random spot along with the stairs up
    (let ((player-loc (random-choice floor-spaces))
          (stairs-up-number (map-cell-number (gethash "stairs-up" *map-cells-by-name*))))
      (place-player (first player-loc) (second player-loc))
      (setf (aref *level* (second player-loc) (first player-loc))
            (list 5 stairs-up-number)))))
  
