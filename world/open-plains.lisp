(in-package #:detome)

(defun build-open-plains ()
  (clear-monsters-from-level)

  (set-perlin-level
   #'(lambda (x y)
       (let ((per (perlin2d x y 0.1 4)))
         (cond
           ((> per 0.75) '(0 2))
           ((> per 0.6) '(0 8))
           (t '(0))))))

  (place-monster "rat" 10 10)

  (place-player 1 1))