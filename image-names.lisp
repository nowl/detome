(in-package :detome)

(defparameter *name-image-lookup* (make-hash-table :test #'equal))

(defun name-image (name)
  (gethash name *name-image-lookup*))

;; name and color
(sh *name-image-lookup*
    "wall" '("cp437-DB" (1 1 0))
    "water" '("cp437-F7" (0 0 1))
    "player" '("cp437-40" (1 1 1)))
