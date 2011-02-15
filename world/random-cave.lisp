(in-package #:detome)

(defun build-random-cave (width height complexity dlevel)
  ;(clear-intensity-map)
  (clear-monsters-from-level)
  (clear-scenery-from-level)
  (clear-items-from-level)
  
  (let ((map
         (let ((pre (generate-dungeon width height complexity)))
           (loop for i below (array-dimension pre 0) collect
                (loop for j below (array-dimension pre 1) collect
                     (if (eq (aref pre i j) :floor)
                         5
                         6))))))
    
    (setf *level-type* :predefined
          *level-width* (length map)
          *level-height* (length (car map))
          *level* (make-array (list (length (car map)) (length map))))
    (loop for i below (length (car map)) do
         (loop for j below (length map) do
              (setf (aref *level* i j)
                    (list (nth j (nth i map)))))))

  (clear-intensity-map)
  (clear-explored-map)

  
  (block set-player
    (loop for x below *level-width* do
         (loop for y below *level-height* do
              (when (equal (aref *level* y x) '(5))
                (place-player x y)
                (return-from set-player nil))))))