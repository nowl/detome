(in-package #:detome)

(defun build-rat-basement ()
  
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
    (5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 1 7 7 1 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 7 7 1 5 5 5 5 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 1 7 7 1 5 5 6 6 5 5 5 6)
    (5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 1 7 7 1 5 5 6 6 5 5 5 6)
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
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 6)))

  (place-player 1 4)

  (make-scenery "tree" 16 6)

  (place-monster "giant rat" 3 2)
  (place-monster "giant rat" 4 2)
  (place-monster "giant rat" 5 2)

  ;; If this place has not been visited before..
  (let ((visited-exists (get-meta :visited *player*)))
    (unless (and visited-exists (member "rat-basement" visited-exists))
      (textarea-log '("You awaken to rats. You have no recollection of how you got here but your head is aching."))
      (set-meta (:visited *player*)
                (append visited-exists '("rat-basement")))
      (clear-explored-map))))

