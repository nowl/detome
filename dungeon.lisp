(in-package #:detome)

(export '(generate-dungeon))

(defvar *build-types*
  '((hallway 10)
    (room 1)
    (floor-space 1)))

(defvar *walls*)

(defun not-edge-tile (dungeon y x)
  (let ((h (array-dimension dungeon 0))
        (w (array-dimension dungeon 1)))
    (and (> y 0) (> x 0) (< y (1- h)) (< x (1- w)))))

(defun add-tile (dungeon type &key i j loc)
  (destructuring-bind (y x) 
      (cond (loc loc)
            ((and i j) (list i j))
            (t (error t "must pass in a valid location, received :i ~a, :j ~a, :loc ~a"
                      i j loc)))
    (log :debug "building a ~a at ~a,~a" type y x)
    (setf (aref dungeon y x) type)
    (when (and (eq type :wall)
               (not-edge-tile dungeon y x))
      (push (list y x) *walls*))))

(defun random-non-edge-dungeon-tile (dungeon)
  (let ((i (1+ (random (- (array-dimension dungeon 0) 2))))
        (j (1+ (random (- (array-dimension dungeon 1) 2)))))
    (list i j)))

(defun build-with-surrounding-when-null (dungeon i j tile surround)
  (loop for x from (1- j) to (1+ j) do
       (loop for y from (1- i) to (1+ i) do
          ;; if designated tile then place it
            (if (and (= i y) (= j x))
                (add-tile dungeon tile :i y :j x)
                ;; otherwise if surrounding tile is nil then place
                ;; surround tile
                (if (null (aref dungeon y x))
                    (add-tile dungeon surround :i y :j x))))))

(defun generate-random-start (dungeon)
  (destructuring-bind (i j) (random-non-edge-dungeon-tile dungeon)
    (build-with-surrounding-when-null dungeon i j :floor :wall)))

(defun select-random-wall (dungeon)
  (let* ((wall-num (random (length *walls*)))
         (wall (nth wall-num *walls*)))
    (log :debug "selecting wall at ~a" wall)
    (setf *walls* (delete wall *walls* :test #'equal :start wall-num))
    (add-tile dungeon nil :loc wall)
    wall))

#|
(defun generate-hallway (dungeon start end)
  (destructuring-bind (s-i s-j) start
    (destructuring-bind (e-i e-j) end
      (log :debug "trying to build hallway between ~a and ~a" start end)
      (let ((points-xy (if (and (= s-i e-i)
                                (= s-j e-j))
                           (list (list s-j s-i))
                           (black::bresenham s-j s-i e-j e-i))))
        ;; first check if each is nil
        (loop for point-xy in points-xy do
             (let* ((i (second point-xy))
                    (j (first point-xy))
                    (tile (aref dungeon i j)))
               (when (not (null tile))
                 (log :debug "failed to build hallway due to ~a" (list i j))
                 (add-tile dungeon :wall :loc start)
                 (return-from generate-hallway nil))))
        ;; build the actual hallway
        (loop for point-xy in points-xy do
             (let ((i (second point-xy))
                   (j (first point-xy)))
               (build-with-surrounding-when-null dungeon i j :hallway :wall)))))))
|#

(defun count-nulls (dungeon loc direction)
  (let ((i (first loc))
        (j (second loc))
        (h (array-dimension dungeon 0))
        (w (array-dimension dungeon 1))
        (count 0))
    (ecase direction
      (:north (loop for y from i above 0 do
                   (if (not (null (aref dungeon y j)))
                       (return-from count-nulls count)
                       (incf count))))
      (:south (loop for y from i below (1- h) do
                   (if (not (null (aref dungeon y j)))
                       (return-from count-nulls count)
                       (incf count))))
      (:east (loop for x from j below (1- w) do
                  (if (not (null (aref dungeon i x)))
                      (return-from count-nulls count)
                      (incf count))))
      (:west (loop for x from j above 0 do
                  (if (not (null (aref dungeon i x)))
                      (return-from count-nulls count)
                      (incf count)))))
    count))


(defun find-random-direction-and-length (dungeon loc)
  (let ((i (first loc))
        (j (second loc))          
        (directions-to-check '(:north :south :east :west)))
    (loop while (not (null directions-to-check)) do
         (let ((direction (random-choice directions-to-check)))
           (setf directions-to-check (remove direction directions-to-check))
           (ecase direction
             (:north (when (null (aref dungeon (1- i) j))
                       (return-from find-random-direction-and-length
                         (values :north (count-nulls dungeon loc :north)))))
             (:south (when (null (aref dungeon (1+ i) j))
                       (return-from find-random-direction-and-length
                         (values :south (count-nulls dungeon loc :south)))))
             (:east (when (null (aref dungeon i (1+ j)))
                      (return-from find-random-direction-and-length
                        (values :east (count-nulls dungeon loc :east)))))
             (:west (when (null (aref dungeon i (1- j)))
                      (return-from find-random-direction-and-length
                        (values :west (count-nulls dungeon loc :west))))))))
    (values nil 0)))


(defun generate-ortho-hallway (dungeon start)
  (destructuring-bind (s-i s-j) start
    (multiple-value-bind (dir length-abs) (find-random-direction-and-length dungeon start)
      (log :debug "trying to build hallway from ~a" start)
      (if (null dir)
          (progn
            (log :debug "failed to build hallway")
            (add-tile dungeon :wall :loc start))
          (progn
            (let ((length (random length-abs)))
              (log :debug "building hallway ~a for ~a tiles" dir length)
              ;; build the actual hallway      
              (ecase dir
                (:north (loop for i below length do
                             (build-with-surrounding-when-null dungeon (- s-i i) s-j :hallway :wall)))
                (:south (loop for i below length do
                             (build-with-surrounding-when-null dungeon (+ s-i i) s-j :hallway :wall)))
                (:east (loop for j below length do
                            (build-with-surrounding-when-null dungeon s-i (+ s-j j) :hallway :wall)))
                (:west (loop for j below length do
                            (build-with-surrounding-when-null dungeon s-i (- s-j j) :hallway :wall))))))))))


(defun generate-random-asset (dungeon)
  (let ((loc (select-random-wall dungeon))
        (type (random-weighted-choice *build-types*)))
    (log :debug "trying to build ~a at ~a" type loc)
    (ecase type
      (room (add-tile dungeon :wall :loc loc))
      (floor-space (build-with-surrounding-when-null dungeon (first loc) (second loc) :floor :wall))
      (hallway (generate-ortho-hallway dungeon loc)))))

(defun generate-dungeon (width height loops)
  (let ((dungeon (make-array (list height width) :initial-element nil)))

    (setf *walls* nil)

    (generate-random-start dungeon)

    ;; build assets
    (loop for loop below loops do
         (generate-random-asset dungeon))

    dungeon))

(defun array-map (array func)
  (let ((new-array (make-array (array-dimensions array))))
    (loop for i below (array-dimension array 0) do
         (loop for j below (array-dimension array 1) do
              (setf (aref new-array i j)
                    (funcall func (aref array i j) i j))))
    new-array))

(defun pretty-print (dungeon)
  (array-map dungeon
             #'(lambda (tile i j)
                 (case tile
                   (:wall 1)
                   (:floor 2)
                   (:hallway 3)
                   (t 0)))))
                     