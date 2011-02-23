(in-package #:detome)

(defun build-rat-basement ()
  
  (clear-monsters-from-level)

  (set-level
   ((6 6 6 6 6 6 6 6 6 6 6 6 6 5 5 5 5 5 5 5 5 5 5 5)
    (6 5 5 5 5 5 5 5 5 5 5 6 6 5 5 5 5 5 5 5 5 5 5 5)
    (6 5 5 5 5 5 6 5 5 5 5 6 6 5 5 5 5 5 5 5 5 5 5 5)
    (6 5 5 5 5 5 6 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (6 5 5 5 5 5 6 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (6 6 6 6 6 6 6 6 6 6 6 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)
    (5 5 5 5 5 5 5 5 6 5 5 5 6 5 5 5 5 5 5 5 5 5 5 5)))


  (place-monster "giant rat" 3 2)
  (place-monster "giant rat" 4 2)
  (place-monster "giant rat" 5 2))