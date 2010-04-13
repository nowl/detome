(in-package #:detome)

(export '(generate-dungeon))

(defvar *build-types*
  '(hallway
	room))	

(defun generate-random-start (dungeon)
  (let ((i (random (array-dimension dungeon 0)))
		(j (random (array-dimension dungeon 1))))
	;;(type (random-choice *build-types*)))
	(log :debug "building a floor at ~a,~a" i j)
	(setf (aref dungeon i j) :floor)))

(defun generate-dungeon (width height loops)
  (let ((dungeon (make-array (list height width) :initial-element nil)))
	
	;; build assets
	(loop for loop below loops do
		 (generate-random-start dungeon))
	
	dungeon))