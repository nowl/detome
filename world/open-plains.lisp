(in-package #:detome)

(defun build-open-plains ()
  (clear-monsters-from-level)
  (clear-scenery-from-level)
  (clear-items-from-level)

  (set-perlin-level
   #'(lambda (x y)
       (let ((per (bf:perlin2d x y 0.1 4)))
         (cond
           ((> per 0.75) (if (= (floor (* 50 (bf:perlin2d x y 1 2))) 0)
                             ;; cave
                             '(0 2 11)
                             '(0 2)))
           ((> per 0.7) '(0 9))
           ((> per 0.6) '(0 8))
           (t (if (= (floor (* 25 (bf:perlin2d x y 1 2))) 0)
                  ;; trader
                  '(0 12)
                  '(0)))))))
#|
  (make-object
   :name "plains monster creator"
   :update-cb #'(lambda (obj)
                  (declare (ignore obj))
                  (when (> (random 1.0) 0.65)
                    (let (x y)
                      (ecase (random 4)
                        (0 (setf x (+ (x *player*) 20)
                                 y (+ (y *player*) (random 30) -15)))
                        (1 (setf x (- (x *player*) 20)
                                 y (+ (y *player*) (random 30) -15)))
                        (2 (setf x (+ (x *player*) (random 30) -15)
                                 y (+ (y *player*) 20)))
                        (3 (setf x (+ (x *player*) (random 30) -15)
                                 y (- (y *player*) 20))))
                      (place-random-monster 0 1 x y))))
   :update-cb-control '(:turns 0))
|#

  (place-player 0 0))

(defun cleanup-open-plains ())
