(in-package #:detome)

(defun attempt-move-monster (monster delta-x delta-y)
  (with-slots (x y) monster
	(let ((new-x (+ x delta-x))
		  (new-y (+ y delta-y)))
	  (when (walkable new-x new-y)
		(setf x new-x y new-y)))))

(defmacro turn-helper (tt-move ttn-move &body body)
  `(let ((ticks-till-next-move (cdr (assoc :ttn-move (meta obj))))
         (ticks-till-move (cdr (assoc :tt-move (meta obj)))))
     (cond ((null ticks-till-next-move)
            (push (cons :tt-move ,tt-move) (meta obj))
            (push (cons :ttn-move ,ttn-move) (meta obj)))
           ((>= ticks-till-next-move ticks-till-move)
            ,@body
            (rplacd (assoc :ttn-move (meta obj)) 0))
           (t (rplacd (assoc :ttn-move (meta obj)) (1+ ticks-till-next-move))))))

(defgeneric random-walk-movement (obj)
  (:method ((obj actor))
    (turn-helper 2 1
      (let ((x (- (random 3) 1))
            (y (- (random 3) 1)))
        (attempt-move-monster obj x y)))))
        

(defgeneric distance (src dest)
  (:method ((src actor) (dest actor))
    (sqrt (+ (expt (- (x src) (x dest)) 2)
             (expt (- (y src) (y dest)) 2)))))

(defun random-walk-movement-with-chase (obj)
  (turn-helper 2 1
    (if (< (distance *player* obj) 10)
        ;; TODO should use astar routine here
        (let ((x (cond ((< (x *player*) (x obj)) -1)
                       ((> (x *player*) (x obj)) 1)
                       (t 0)))
              (y (cond ((< (y *player*) (y obj)) -1)
                       ((> (y *player*) (y obj)) 1)
                       (t 0))))
          (attempt-move-monster obj x y))
        (let ((x (- (random 3) 1))
              (y (- (random 3) 1)))
          (attempt-move-monster obj x y)))))
  
  

(define-monster-type
	"rat"
	"rat"
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
  #'random-walk-movement-with-chase)
