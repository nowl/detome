(in-package #:blacker)

(export 'random-choice)

(defun random-choice (&rest choices)
  (let ((choice-num (random (length choices))))
    (elt choices choice-num)))