(in-package #:detome)

(defvar *map-cells-by-number* (make-hash-table :test #'eq))
(defvar *map-cells-by-name* (make-hash-table :test #'equal))

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
  :attenuation '(0.1 :dark 0.6)
  :walkable t
  :image "plain")
(define-map-cell 1
    "wall"
  :attenuation 1.0
  :walkable nil
  :image "wall")
(define-map-cell 2
    "mountain"
  :attenuation '(0.75 :dark 0.9)
  :walkable t
  :image "mountain")
(define-map-cell 3
    "tree-on-plain"
  :attenuation '(0.1 :dark 0.6)
  :walkable t
  :image '("plain" "tree"))
(define-map-cell 4
    "tree-on-mountain"
  :attenuation '(0.75 :dark 0.9)
  :walkable t
  :image '("mountain" "tree"))
