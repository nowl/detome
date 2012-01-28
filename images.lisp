(in-package #:detome)

(defun define-images ()
  (bf:define-image "cave" "data/tileset.png" `(1 ,(1+ (* 3 33)) 32 32))
  (bf:define-image "trader" "data/tileset.png" `(,(1+ (* 1 33)) ,(1+ (* 3 33)) 32 32))
  (bf:define-image "stairs-up" "data/tileset.png" `(1 ,(1+ (* 5 33)) 32 32))
  (bf:define-image "stairs-down" "data/tileset.png" `(,(1+ (* 1 33)) ,(1+ (* 5 33)) 32 32))

  (bf:define-image "plain" "data/tileset.png" '(1 1 32 32))
  (bf:define-image "tree-1" "data/tileset.png" '(100 1 32 32))
  (bf:define-image "tree-2" "data/tileset.png" `(,(1+ (* 9 33)) 1 32 32))
  (bf:define-image "wall" "data/tileset.png" '(34 1 32 32))
  (bf:define-image "mountain" "data/tileset.png" '(67 1 32 32))
  (bf:define-image "player-front" "data/tileset.png" '(1 34 32 32))

  (bf:define-image "rat" "data/tileset.png" `(,(1+ (* 0 33)) ,(1+ (* 2 33)) 32 32))
  (bf:define-image "giant-rat" "data/tileset.png" `(,(1+ (* 1 33)) ,(1+ (* 2 33)) 32 32))
  (bf:define-image "spider" "data/tileset.png" `(,(1+ (* 2 33)) ,(1+ (* 2 33)) 32 32))

  (bf:define-image "dirty-cement-floor" "data/tileset.png" '(133 1 32 32))
  (bf:define-image "rock-wall" "data/tileset.png" '(166 1 32 32))
  (bf:define-image "dust-1" "data/tileset.png" `(,(1+ (* 6 33)) 1 32 32))
  (bf:define-image "dust-2" "data/tileset.png" `(,(1+ (* 7 33)) 1 32 32))
  (bf:define-image "plain-2" "data/tileset.png" `(,(1+ (* 8 33)) 1 32 32))

  ;; items
  (bf:define-image "green energy" "data/tileset.png" `(1 ,(1+ (* 4 33)) 32 32))
  (bf:define-image "blue energy" "data/tileset.png" `(,(1+ (* 1 33)) ,(1+ (* 4 33)) 32 32))
  (bf:define-image "red energy" "data/tileset.png" `(,(1+ (* 2 33)) ,(1+ (* 4 33)) 32 32))
  (bf:define-image "yellow energy" "data/tileset.png" `(,(1+ (* 3 33)) ,(1+ (* 4 33)) 32 32)))
