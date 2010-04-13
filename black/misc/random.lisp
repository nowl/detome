(in-package #:black)

(export '(random-choice))

(defun random-choice (list)
  (nth (random (length list))
	   list))