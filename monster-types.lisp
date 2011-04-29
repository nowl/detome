(in-package #:detome)

(defun attempt-move-monster (monster delta-x delta-y)
  (with-slots (x y) monster
	(let ((new-x (+ x delta-x))
		  (new-y (+ y delta-y)))
	  (when (walkable new-x new-y)        
		(setf x new-x y new-y)))))

(defmacro turn-helper (tt-move ttn-move &body body)
  `(let ((ticks-till-next-move (get-meta :ttn-move obj))
         (ticks-till-move (get-meta :tt-move obj)))
     (cond ((null ticks-till-next-move)
            (set-meta (:tt-move obj) ,tt-move)
            (set-meta (:ttn-move obj) ,ttn-move))
           ((>= ticks-till-next-move ticks-till-move)
            ,@body
            (set-meta (:tt-move obj) 0))
           (t (set-meta (:ttn-move obj) (1+ ticks-till-next-move))))))

(defgeneric random-walk-movement (obj)
  (:method ((obj actor))
    (turn-helper 2 1
      (if (<= (distance *player* obj) 1)
          (attack obj *player*)
          (let ((x (- (random 3) 1))
                (y (- (random 3) 1)))
            (attempt-move-monster obj x y))))))

(defgeneric distance (src dest)
  (:method ((src actor) (dest actor))
    (sqrt (+ (expt (- (x src) (x dest)) 2)
             (expt (- (y src) (y dest)) 2)))))

(defun random-walk-movement-with-chase (obj)
  (turn-helper 2 1
    (cond ((<= (distance *player* obj) 2)
           (attack obj *player*))
          ((< (distance *player* obj) 10)
           ;; TODO should use astar routine here
           (let ((x (cond ((< (x *player*) (x obj)) -1)
                          ((> (x *player*) (x obj)) 1)
                          (t 0)))
                 (y (cond ((< (y *player*) (y obj)) -1)
                          ((> (y *player*) (y obj)) 1)
                          (t 0))))
             ;;(textarea-log `("The " (:color "ff0000") ,(name (mon-type obj)) (:color "ffffff") " yells, \"You won't escape!\""))
             (attempt-move-monster obj x y))
           (let ((x (- (random 3) 1))
                 (y (- (random 3) 1)))
             (attempt-move-monster obj x y))))))  

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-drop-table-aux (val form)
    `((<= ,val ,(second form)) ,(first form))))

(defmacro make-drop-table (val alist)
  `(cond ,@(loop for form in alist collect
                 (make-drop-table-aux val form))
         (t nil)))

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
  #'random-walk-movement
  #'(lambda (mob)
      (declare (ignore mob))
      (list
       (let ((val (random 1.0)))
         (make-drop-table 
          val
          (("green energy" 0.3)))))))

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
  #'random-walk-movement-with-chase
  #'(lambda (mob)
      (declare (ignore mob))
      (list
       (let ((val (random 1.0)))
         (make-drop-table
          val
          (("yellow energy" 0.1)
           ("red energy" 0.2)
           ("blue energy" 0.6)
           ("green energy" 0.9)))))))
