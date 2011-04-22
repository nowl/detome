(in-package #:detome)

(defparameter *map-cells-by-number* (make-hash-table :test #'eq))
(defparameter *map-cells-by-name* (make-hash-table :test #'equal))

(defstruct map-cell
  number
  name
  attenuation
  walkable
  image)

(defmacro define-map-cell (number name &key attenuation walkable image)
  (let ((cell (gensym)))    
    `(let ((,cell (make-map-cell :number ,number
                                 :attenuation ,attenuation
                                 :walkable ,walkable
                                 :image ,image)))
       (setf (gethash ,number *map-cells-by-number*)
             ,cell
             (gethash ,name *map-cells-by-name*)
             ,cell))))

(define-map-cell 0
    "plain"
  :attenuation 0.01
  :walkable t
  :image #'(lambda (x y)
             (case (floor (* 2 (perlin2d x y 1 4)))
               (0 "plain")
               (t "plain-2"))))
(define-map-cell 1
    "wall"
  :attenuation 1.0
  :walkable nil
  :image "wall")
(define-map-cell 2
    "mountain"
  :attenuation 0.75
  :walkable t
  :image "mountain")
(define-map-cell 5
    "dirty-cement-floor"
  :attenuation 0.1
  :walkable t
  :image "dirty-cement-floor")
(define-map-cell 6
    "rock-wall"
  :attenuation 1.0
  :walkable nil
  :image "rock-wall")
(define-map-cell 8
  "tree"
  :attenuation 0.1
  :walkable t
  :image #'(lambda (x y)
             (case (floor (* 2 (perlin2d x y 1.1 4)))
               (0 "tree-1")
               (t "tree-2"))))
(define-map-cell 9
  "dust-1"
  :attenuation 0.1
  :walkable t
  :image "dust-1")
(define-map-cell 10
  "dust-2"
  :attenuation 0.1
  :walkable t
  :image "dust-2")
