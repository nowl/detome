(in-package #:detome)

(defun build-rat-basement ()
  (clear-intensity-map)
  (clear-monsters-from-level)

  (set-level
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

  (loop for x from 6 below 16 do
       (make-scenery "tree" 16 x))

  (place-monster "rat" 2 1)
  (place-monster "rat" 3 2)
  (place-monster "rat" 4 2)
  (place-monster "rat" 4 3)
  (place-monster "rat" 5 4)
  (place-monster "giant rat" 6 1)

  ;; If this place has not been visited before..
  (let ((visited-exists (get-meta :visited *player*)))
    (unless (and visited-exists (member "rat-basement" visited-exists))
      (textarea-log '("You awaken to rats. You have no recollection of how you got here but your head is aching."))
      (set-meta (:visited *player*)
                (append visited-exists '("rat-basement")))
      (clear-explored-map))))

