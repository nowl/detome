(in-package #:black)

(export '(random-choice random-weighted-choice))

(defun random-choice (list)
  (nth (random (length list))
	   list))

(defun random-weighted-choice (list)
  (let* ((sum (loop for (val weight) in list summing weight))
         (target (random (float sum))))
    (loop for (val weight) in list do
         (decf target weight)
         (when (minusp target)
           (return-from random-weighted-choice val)))))
    