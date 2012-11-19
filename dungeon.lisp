(in-package #:detome)

(shadow '(room))

#|
(defun find-closest-room (rooms start-x start-y)
  (loop with closest = (car rooms) for room in (cdr rooms) do
        (let ((curr-dist (+ (expt (- (first closest) start-x) 2)
                            (expt (- (second closest) start-y) 2)))
              (this-dist (+ (expt (- (first room) start-x) 2)
                            (expt (- (second room) start-y) 2))))
          (when (< this-dist curr-dist)
            (setf closest room)))
     finally (return closest)))
|#

(defstruct room
  dungeon-section-x
  dungeon-section-y
  width
  height
  start-x
  start-y)

(defstruct dungeon
  cells
  (rooms nil :type list))

(defconstant +directions+ (list :north :south :east :west))

(defun randomly-walk (dungeon x y min-passage-length max-passage-length incoming-dir out-of-room)
  (declare (fixnum x y min-passage-length max-passage-length)
           (boolean out-of-room)
           (symbol incoming-dir))
  (let ((passage-len (+ (random (1+ (- max-passage-length min-passage-length))) min-passage-length))
        (dir (apply #'random-choice (remove incoming-dir +directions+))))
    (destructuring-bind (next-x next-y)
        (loop with new-x = x with new-y = y
           for i below passage-len do
             (setf new-x (case dir (:east (1+ new-x)) (:west (1- new-x)) (t new-x))
                   new-y (case dir (:north (1- new-y)) (:south (1+ new-y)) (t new-y)))
             ;;(format t "x: ~a y: ~a dir: ~a out: ~a~%" new-x new-y dir out-of-room)
             (when (or (< new-x 0) (< new-y 0)
                       (>= new-x (array-dimension (dungeon-cells dungeon) 1))
                       (>= new-y (array-dimension (dungeon-cells dungeon) 0)))
               (return-from randomly-walk nil))
             (when (and out-of-room (/= (aref (dungeon-cells dungeon) new-y new-x) 0))
               (return-from randomly-walk nil))
             (when (and (not out-of-room) (= (aref (dungeon-cells dungeon) new-y new-x) 0))
               (setf out-of-room t))
             (setf (aref (dungeon-cells dungeon) new-y new-x) 2)
           finally (return (list new-x new-y)))
      (let ((reverse-dir (case dir (:north :south) (:east :west) (:south :north) (:west :east))))
        (randomly-walk dungeon next-x next-y min-passage-length max-passage-length reverse-dir out-of-room)))))

(defun make-connections (dungeon section-width section-height min-passage-length max-passage-length min-num-exits max-num-exits)
  (declare (dungeon dungeon)
           (fixnum min-passage-length max-passage-length
                   min-num-exits max-num-exits))
  (let ((unvisited-rooms (copy-list (dungeon-rooms dungeon))))
    (loop for i below (length unvisited-rooms) do
         (let* ((room-num (random (length unvisited-rooms)))
                (room (elt unvisited-rooms room-num)))
           (setf unvisited-rooms (delete room unvisited-rooms :test #'equal))
           (let ((num-exits (+ (random (1+ (- max-num-exits min-num-exits))) min-num-exits)))
             ;; pick a random spot in room and randomly walk
             (loop for exit-num below num-exits do
                  (let ((starting-x (+ (* (room-dungeon-section-x room) section-width)
                                       (+ (room-start-x room) (random (room-width room)))))
                        (starting-y (+ (* (room-dungeon-section-y room) section-height)
                                       (+ (room-start-y room) (random (room-height room))))))
                    (randomly-walk dungeon starting-x starting-y
                                   min-passage-length max-passage-length
                                   (apply #'random-choice +directions+)
                                   nil))))))))
  
(defun build-cells (dungeon &key cells-width cells-height)
  (declare (dungeon dungeon)
           (fixnum cells-width cells-height))
  (setf (dungeon-cells dungeon)
        (make-array (list cells-height cells-width))
        (dungeon-rooms dungeon)
        nil))

(defun fill-room (dungeon room &key
                  min-room-width max-room-width
                  min-room-height max-room-height
                  section-width section-height)
  (declare (dungeon dungeon)
           (room room)
           (fixnum min-room-height max-room-height
                   min-room-width max-room-width
                   section-width section-height))
  (let ((width  (+ min-room-width  (random (1+ (- max-room-width  min-room-width)))))
        (height (+ min-room-height (random (1+ (- max-room-height min-room-height))))))
    (let ((x-start (random (1+ (- section-width width))))
          (y-start (random (1+ (- section-height height)))))
      (loop for i from y-start below (+ y-start height) do
           (loop for j from x-start below (+ x-start width) do
                (setf (aref (dungeon-cells dungeon) 
                            (+ (* (room-dungeon-section-y room) section-height) i)
                            (+ (* (room-dungeon-section-x room) section-width) j))
                      1)))
      (setf (room-width room) width
            (room-height room) height
            (room-start-x room) x-start
            (room-start-y room) y-start))))

(defun build-rooms (dungeon num-x-sections num-y-sections section-width section-height
                    min-room-width max-room-width min-room-height max-room-height room-prob)
  (declare (dungeon dungeon)
           (fixnum num-x-sections num-y-sections section-width section-height)
           (float room-prob))  
  (loop for y below num-y-sections do
       (loop for x below num-x-sections do
            (when (<= (random 1.0) room-prob)
              (let ((new-room (make-room :dungeon-section-x x :dungeon-section-y y)))
                (fill-room dungeon new-room
                           :min-room-width min-room-width
                           :max-room-width max-room-width
                           :min-room-height min-room-height
                           :max-room-height max-room-height
                           :section-width section-width
                           :section-height section-height)
                (push new-room (dungeon-rooms dungeon)))))))

(defun create-dungeon (&key num-x-sections num-y-sections section-width section-height
                       min-room-width max-room-width min-room-height max-room-height room-prob
                       min-passage-length max-passage-length min-num-exits max-num-exits)
  (declare (fixnum num-x-sections num-y-sections section-width section-height
                   min-room-width max-room-width min-room-height max-room-height
                   min-passage-length max-passage-length min-num-exits max-num-exits)
           (single-float room-prob))
  (let ((dungeon (make-dungeon)))
    (build-cells dungeon
                 :cells-width (* num-x-sections section-width)
                 :cells-height (* num-y-sections section-height))
    (build-rooms dungeon num-x-sections num-y-sections section-width section-height
                 min-room-width max-room-width min-room-height max-room-height
                 room-prob)
    (make-connections dungeon section-width section-height min-passage-length max-passage-length
                      min-num-exits max-num-exits)
    dungeon))

(defun dungeon-view (dungeon x y width height)
  (let* ((cells (dungeon-cells dungeon))
         (view (make-array (list (min height (- (array-dimension cells 0) y))
                                 (min width (- (array-dimension cells 1) x))))))
    (loop for i from 0 below (array-dimension view 0) do
         (loop for j from 0 below (array-dimension view 1) do
              (setf (aref view i j)
                    (aref cells (+ y i) (+ x j)))))
    view))

(defun pick-random-room (dungeon)
  (let* ((rooms (dungeon-rooms dungeon))
         (rand-dung (random 

(defun pick-random-spot-in-room