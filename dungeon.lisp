(in-package #:detome)

(defun connect-cells (cell1 cell2)
  (let ((x-diff (- (first cell2) (first cell1)))
        (y-diff (- (second cell2) (second cell1)))
        choices)
    (when (/= 0 x-diff) (push :x choices))
    (when (/= 0 y-diff) (push :y choices))
    
    (let* ((choice-n (random (length choices)))
           (choice (nth choice-n choices)))
      (if (eq choice :y)
          ;; do vertical
          (let* ((run (* (signum y-diff) (1+ (random (abs y-diff)))))
                 (next-cell (list (first cell1) (+ (second cell1) run))))
            ;;(format t "vert 1:~a 2:~a next:~a~%" cell1 cell2 next-cell)
            (cons cell1 (if (equal next-cell cell2)
                            (list cell2)
                            (connect-cells next-cell cell2))))
          ;; do horizontal
          (let* ((run (* (signum x-diff) (1+ (random (abs x-diff)))))
                 (next-cell (list (+ (first cell1) run) (second cell1))))
            ;;(format t "horz 1:~a 2:~a next:~a~%" cell1 cell2 next-cell)
            (cons cell1 (if (equal next-cell cell2)
                            (list cell2)
                            (connect-cells next-cell cell2))))))))

;; returns list of connections to create
(defun connect-rooms (room1 room2)
  (let ((cell-connections (connect-cells (subseq room1 0 2)
                                         (subseq room2 0 2))))
    cell-connections))

(defun find-closest-room (rooms start-x start-y)
  (loop with closest = (car rooms) for room in (cdr rooms) do
        (let ((curr-dist (+ (expt (- (first closest) start-x) 2)
                            (expt (- (second closest) start-y) 2)))
              (this-dist (+ (expt (- (first room) start-x) 2)
                            (expt (- (second room) start-y) 2))))
          (when (< this-dist curr-dist)
            (setf closest room)))
        finally (return closest)))

(defun create-dungeon (num-x-cells num-y-cells max-cell-width max-cell-height min-cell-width min-cell-height room-prob)
  (let ((dungeon (make-array (list (* num-y-cells max-cell-height) (* num-x-cells max-cell-width))))
        rooms)
    (flet ((fill-cell (x y)
                      (let ((width (+ min-cell-width (random (1+ (- max-cell-width min-cell-width)))))
                            (height (+ min-cell-height (random (1+ (- max-cell-height min-cell-height))))))
                        (let ((x-start (random (1+ (- max-cell-width width))))
                              (y-start (random (1+ (- max-cell-height height)))))
                          (loop for i from y-start below (+ y-start height) do
                                (loop for j from x-start below (+ x-start width) do
                                      (setf (aref dungeon (+ (* y max-cell-height) i) (+ (* x max-cell-width) j)) 1)))
                          (list x-start y-start width height))))
           (make-path-connecting-rooms ()
                                       (loop for room in rooms do
                                             (let ((x (first room))
                                                   (y (second room)))
                                               (format t "closest room to ~a, ~a: ~a~%" x y
                                                       (find-closest-room (remove room rooms :test #'equal) x y))))))
      (loop for y below num-y-cells do
            (loop for x below num-x-cells do
                  (when (<= (random 1.0) room-prob)
                    (let ((params (fill-cell x y)))
                      (push (append (list x y) params) rooms)))))
      ;;(let ((width (random 
      (make-path-connecting-rooms)
      (list dungeon rooms))))