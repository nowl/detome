(in-package #:detome)

(defun build-rat-basement ()
  ;(clear-intensity-map)
  (clear-monsters-from-level)
  (clear-scenery-from-level)
  (clear-items-from-level)

  (set-predefined-level
   ((6 6 6 6 6 6 6 6 6 6 6 6 6 5 5 5 5 5 5 5 5 5 5 5 6 5 6 6 6 6 6 6)
    (6 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (6 5 5 5 5 5 6 5 5 5 5 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (6 5 5 5 5 5 6 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (6 5 5 5 5 5 6 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (6 6 6 6 6 6 6 6 6 6 6 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 1 5 5 1 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 5 5 1 5 5 5 5 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 1 5 5 1 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 1 5 5 1 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 1 1 1 1 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)))

  (place-player 1 4)

  (loop for x below *level-width* do
       (loop for y below *level-height* do
            (if (and (> (bf:perlin2d x y 1 4) 0.6) (equal (aref *level* y x) '(5)))
                (make-scenery (bf:random-choice '("dust-1" "dust-2")) x y))))

  (place-monster "rat" 2 1)
  (place-monster "rat" 3 2)
  (place-monster "rat" 4 2)
  (place-monster "rat" 4 3)
  (place-monster "rat" 5 4)
  (place-monster "giant rat" 6 1)

  (loop for x below *level-width* do
       (loop for y below *level-height* do
            (when (eql (car (aref *level* y x)) 5)
              (cond ((> (bf:perlin2d x y 1 2) 0.95) (place-monster "giant rat" x y))
                    ((> (bf:perlin2d x y 1 2) 0.9) (place-monster "rat" x y))))))

  ;; If this place has not been visited before..
  (let ((visited-exists (get-meta :visited *player*)))
    (unless (and visited-exists (member "rat-basement" visited-exists))
      ;(textarea-log '("You awaken to rats. You have no recollection of how you got here but your head is aching."))
      (set-meta :visited 
                (append visited-exists '("rat-basement"))
                *player*)
      ;(clear-explored-map))))
      )))