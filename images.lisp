(in-package #:detome)

(defun define-images ()
  (define-image "message-exclamation" "data/tileset.png" `(1 ,(1+ (* 3 33)) 32 32))
  (define-image "plain" "data/tileset.png" '(1 1 32 32))
  (define-image "tree" "data/tileset.png" '(100 1 32 32))
  (define-image "wall" "data/tileset.png" '(34 1 32 32))
  (define-image "mountain" "data/tileset.png" '(67 1 32 32))
  (define-image "player-front" "data/tileset.png" '(1 34 32 32))
  (define-image "rat" "data/tileset.png" '(1 67 32 32))
  (define-image "giant-rat" "data/tileset.png" '(34 67 32 32))
  (define-image "dirty-cement-floor" "data/tileset.png" '(133 1 32 32))
  (define-image "rock-wall" "data/tileset.png" '(166 1 32 32))
  (define-image "dust-1" "data/tileset.png" `(,(1+ (* 6 33)) 1 32 32))
  (define-image "dust-2" "data/tileset.png" `(,(1+ (* 7 33)) 1 32 32))

  ;; items
  (define-image "rat chunk" "data/tileset.png" `(1 ,(1+ (* 4 33)) 32 32))
  (define-image "attack powerup" "data/tileset.png" `(,(1+ (* 1 33)) ,(1+ (* 4 33)) 32 32)))
