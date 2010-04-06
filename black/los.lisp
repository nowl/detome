(in-package #:black)

(export '(line-of-sight))

;;;; modified ray casting method for line of sight

(defun bresenham (s-x s-y d-x d-y)
  (let (acc)
    (let ((x0 s-x) (x1 d-x)
          (y0 s-y) (y1 d-y))		
      (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
        (when steep
          (setf x0 s-y y0 s-x
                x1 d-y y1 d-x))
        (let ((needs-reverse (> x0 x1)))
          (when needs-reverse
            (let ((tmp 0.0))
              (setf tmp x0
                    x0 x1
                    x1 tmp
                    tmp y0
                    y0 y1
                    y1 tmp)))	  
          (let* ((deltax (- x1 x0))
                 (deltay (abs (- y1 y0)))
                 (err 0.0)
                 (deltaerr (float (/ deltay deltax)))
                 (ystep (if (< y0 y1) 1 -1))
                 (y y0))
            (declare (single-float deltaerr err))
            (loop for x from x0 to x1 do
                 (if steep
                     (push (list y x) acc)
                     (push (list x y) acc))
                 (incf err deltaerr)
                 (when (>= err 0.5)
                   (incf y ystep)
                   (decf err 1.0))))
          (if needs-reverse
              acc
              (nreverse acc)))))))

;; This is to ensure that there are no diagonal leaps in the line. It
;; tends to lead to less breakage in the LOS and makes it a bit more
;; restrictive.
(defun mod-bresenham (s-x s-y d-x d-y)
  (flet ((dist (p1 p2)
           (+ (abs (- (first p1) (first p2)))
              (abs (- (second p1) (second p2))))))
    (let ((line (bresenham s-x s-y d-x d-y))
          mod-line)
      (loop for p1 in line for p2 in (cdr line) do
           (push p1 mod-line)
           (when (and (not (null p2)) (> (dist p1 p2) 1))
             (push (list (first p1) (second p2)) mod-line)))
      (push (car (last line)) mod-line)
      (nreverse mod-line))))

(defvar *search-radius* 20)

(defun build-distance-offsets ()
  (flet ((distance-to (x y)
           (sqrt (+ (* x x) (* y y)))))
    (remove '(0 0) 
            (loop for x from (- *search-radius*) to *search-radius* append
                 (loop for y from (- *search-radius*) to *search-radius*
                    when (<= (distance-to x y) *search-radius*) collect (list x y)))
            :test #'equal)))

(defvar *squares-to-test* (build-distance-offsets))

(defun trace-light-intensity (x y src-x src-y attenuation-key)
  (let ((path (butlast (#+mod-bresenham
                        mod-bresenham
                        bresenham src-x src-y x y)))
        (intensity 1.0))
    (mapcar #'(lambda (point)
                (destructuring-bind (x y) point
                  (setf intensity (* intensity
                                     (- 1 (funcall attenuation-key x y))))))
            (rest path))
    intensity))

(defun line-of-sight (x y map-width map-height attenuation-key min-intensity)
  "line-of-sight should take an x and y position and return a list of
  'visible' map positions and their relative intensities compared to
  the starting point. attenuation-key is a function that takes x and y
  coordinates and returns the factor the square attenuates light
  by. For example 0 would mean the square totally blocks all light
  whereas 0.5 would halve the light moving into the
  square. min-intensity is the light intensity where any intensity
  below which is deemed 'not visible'."
  (declare (fixnum x y map-width map-height)
           (short-float min-intensity)
           (function attenuation-key))
  (let (results)
    (loop for offset in *squares-to-test* do
         (destructuring-bind (off-x off-y) offset
           (let ((d-x (+ off-x x))
                 (d-y (+ off-y y)))
             (unless (or (< d-x 0) (< d-y 0) (>= d-x map-width) (>= d-y map-height))
               (let ((final-intensity (trace-light-intensity d-x d-y x y attenuation-key)))
                 (when (> final-intensity min-intensity)
                   (push (list d-x d-y final-intensity) results)))))))
    results))