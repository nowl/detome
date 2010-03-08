(in-package #:detome)

(defun attempt-move-monster (monster delta-x delta-y)
  (with-slots (x y) monster
    (let ((new-x (+ x delta-x))
	  (new-y (+ y delta-y)))
      (when (walkable new-x new-y)
        (setf x new-x y new-y)))))

(defgeneric random-walk-movement (obj)
  (:method ((obj actor))
    (let ((ticks-till-next-move (cdr (assoc :ttn-move (meta obj))))
          (ticks-till-move (cdr (assoc :tt-move (meta obj)))))
      (cond ((null ticks-till-next-move)
             (push (cons :tt-move 2) (meta obj))
             (push (cons :ttn-move 1) (meta obj)))
            ((>= ticks-till-next-move ticks-till-move)
             (let ((x (- (random 3) 1))
                   (y (- (random 3) 1)))
               (attempt-move-monster obj x y))
             (rplacd (assoc :ttn-move (meta obj)) 0))
            (t (rplacd (assoc :ttn-move (meta obj)) (1+ ticks-till-next-move)))))))

(define-monster-type
	"rat"
	"giant-rat"
	1
  #'(lambda ()
      (+ 3 (random 5)))
  #'(lambda ()
      (list 1 (1+ (random 5))))
  #'(lambda ()
      (list 1 (1+ (random 5))))
  #'(lambda ()
      (list 1 (1+ (random 5))))
  #'random-walk-movement)

(define-monster-type
	"giant rat"
	"giant-rat"
	2
  #'(lambda ()
      (+ 4 (random 8)))
  #'(lambda ()
      (list 1 (1+ (random 8))))
  #'(lambda ()
      (list 1 (1+ (random 8))))
  #'(lambda ()
      (list 1 (1+ (random 8))))
  ;;#'(lambda (obj)
  ;;    nil))
  #'random-walk-movement)

  
  